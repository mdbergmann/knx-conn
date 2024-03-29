(defpackage :knx-conn.knx-client
  (:use :cl :ip-client :knxobj :hpai :dpt :cemi :address
   :descr-info :connect :tunnelling
        :sento.future)
  (:nicknames :knx-client)
  (:import-from #:sento.actor
                #:!
                #:?
                #:reply)
  (:export #:retrieve-descr-info
           #:establish-tunnel-connection
           #:close-tunnel-connection
           #:tunnel-connection-established-p
           #:send-connection-state
           #:send-write-request
           #:send-read-request
           ;; listeners
           #:add-tunnelling-request-listener
           #:rem-tunnelling-request-listener
           #:clr-tunnelling-request-listeners
           ;; shared vars
           #:*receive-knx-data-recur-delay-secs*
           #:*default-receive-knx-data-recur-delay-secs*
           #:*resp-wait-timeout-secs*
           ;; async handler
           #:*async-handler*
           #:async-handler-receive
           #:start-async-receive
           #:make-async-handler
           #:start-heartbeat
           ;; conditions
           #:knx-receive-error
           #:knx-response-timeout-error
           ))

(in-package :knx-conn.knx-client)

(defvar *async-handler* nil
  "The async communication handler, an actor. It is set in a dependency inversed manner.")

;; ---------------------------------
;; conditions
;; ---------------------------------

(define-condition knx-receive-error (simple-error) ()
  (:report (lambda (c s)
             (format s "KNX receive error: ~a" (simple-condition-format-control c)))))

(define-condition knx-response-timeout-error (simple-error) ()
  (:report (lambda (c s)
             (format s "KNX timeout error: ~a" (simple-condition-format-control c)))))

;; ---------------------------------
;; global variables
;; ---------------------------------

(defparameter *resp-wait-timeout-secs* 3
  "Timeout for waiting for a response.")

(defparameter *heartbeat-resp-wait-timeout-secs* 10
  "Timeout for waiting for a heartbeat response.")

(defparameter *default-receive-knx-data-recur-delay-secs* 0
  "Default delay in seconds for the recurring retrieval of KNX data.
The default is 0 because there should be no delay.")

(defparameter *receive-knx-data-recur-delay-secs*
  *default-receive-knx-data-recur-delay-secs*
  "Defines a delay in seconds for the recurring retrieval of KNX data.
Only applicable if `start-receive` is true in `knx-conn-init`.")

(defconstant +default-heartbeat-interval-secs+ 60
  "Default interval in seconds for sending a connection-state request to the KNXnet/IP gateway.")

(defparameter *heartbeat-interval-secs* +default-heartbeat-interval-secs+
  "Interval in seconds for sending a connection-state request to the KNXnet/IP gateway.")

(defvar *channel-id* nil
  "The channel-id of the current tunnelling connection.")

(defvar *seq-counter* 0
  "The sequence counter for the current tunnelling connection.")

(defvar *tunnel-request-listeners* nil
  "A list of functions to be called when a tunnelling request is received.")

(defvar *awaited-things* (make-hash-table :test #'eq)
  "Pool of received messages that are not handled otherwise.")

;; ----------- helper functions ------------

(defun %assert-channel-id ()
  (assert (integerp *channel-id*)
          nil "No open connection!"))

(defun %next-seq-counter ()
  (setf *seq-counter* (mod (1+ *seq-counter*) 255)))

(defun add-tunnelling-request-listener (listener-fun)
  (! *async-handler* `(:add-tunnel-req-listener . ,listener-fun)))

(defun rem-tunnelling-request-listener (listener-fun)
  (! *async-handler* `(:rem-tunnel-req-listener . ,listener-fun)))

(defun clr-tunnelling-request-listeners ()
  (! *async-handler* '(:clr-tunnel-req-listeners)))

;; ---------------------------------
;; knx-ip protocol functions
;; ---------------------------------

(defun %handle-response-fut (fut handle-fun)
  (fcompleted fut
      (result)
    (destructuring-bind (response _err) result
      (declare (ignore _err))
      (when response
        (funcall handle-fun response))
      (unless response
        (log:warn "No response received.")))))

(defun retrieve-descr-info ()
  "Retrieve the description information from the KNXnet/IP gateway. The response to this request will be received asynchronously.
Returns a future. The result will be a list of the received response and an error condition, if any.
The error condition will be of type `knx-receive-error` and reflects just an error of transport or parsing. The response itself may contain an error status of the KNX protocol."
  (log:info "Retrieving description information...")
  (! *async-handler* `(:send
                       . ,(make-descr-request *hpai-unbound-addr*)))
  (? *async-handler* `(:wait-on-resp
                       . (knx-descr-response
                          ,(get-universal-time)
                          ,*resp-wait-timeout-secs*))))

(defun establish-tunnel-connection ()
  "Send a tunnelling connection to the KNXnet/IP gateway. The response to this request will be received asynchronously.
Returns a future. The result will be a list of the received response and an error condition, if any.
The error condition will be of type `knx-receive-error` and reflects just an error of transport or parsing. The response itself may contain an error status of the KNX protocol.

If the connection is established successfully, the channel-id will be stored in the global variable `*channel-id*`."
  (log:info "Establishing tunnel connection...")
  (! *async-handler* `(:send . ,(make-connect-request)))
  (let ((fut
          (? *async-handler*
             `(:wait-on-resp
               . (knx-connect-response
                  ,(get-universal-time)
                  ,*resp-wait-timeout-secs*)))))
    (%handle-response-fut
     fut
     (lambda (response)
       (let ((status (connect-response-status response)))
         (if (not (eql status 0))
             (log:warn "Tunnel connection failed, status: ~a" status)
             (progn
               (log:info "Tunnel connection established.")
               (log:info "Channel-id: ~a" (connect-response-channel-id response))
               (setf *channel-id*
                     (connect-response-channel-id response)))))))
    fut))

(defun close-tunnel-connection ()
  (%assert-channel-id)
  (log:info "Closing tunnel connection...")
  (! *async-handler* `(:send . ,(make-disconnect-request *channel-id*)))
  (let ((fut
          (? *async-handler*
             `(:wait-on-resp
               . (knx-disconnect-response
                  ,(get-universal-time)
                  ,*resp-wait-timeout-secs*)))))
    (%handle-response-fut
     fut
     (lambda (response)
       (let ((status (disconnect-response-status response)))
         (if (not (eql status 0))
             (log:warn "Tunnel disconnection failed, status: ~a" status)
             (progn
               (log:info "Tunnel connection closed.")
               (setf *channel-id* nil))))))
    fut))

(defun tunnel-connection-established-p ()
  "Returns true if a tunnel connection is established."
  (integerp *channel-id*))

(defun send-connection-state ()
  "Sends a connection-state request to the KNXnet/IP gateway. The response to this request will be received asynchronously.
Returns the request that was sent.
This request should be sent every some seconds (i.e. 60) as a heart-beat to keep the connection alive."
  (%assert-channel-id)
  (! *async-handler* `(:send
                       . ,(make-connstate-request *channel-id*)))
  (? *async-handler* `(:wait-on-resp
                       . (knx-connstate-response
                          ,(get-universal-time)
                          ,*resp-wait-timeout-secs*))))

;; ---------------------------------
;; tunnelling-request functions
;; ---------------------------------

(defun send-write-request (group-address dpt)
  "Send a tunnelling-request as L-Data.Req with APCI Group-Value-Write to the given `address:knx-group-address` with the given data point type to be set.
Returns the request that was sent."
  (check-type group-address knx-group-address)
  (check-type dpt dpt)
  (%assert-channel-id)
  (let ((req (make-tunnelling-request
              :channel-id *channel-id*
              :seq-counter (%next-seq-counter)
              :cemi (make-default-cemi
                     :message-code +cemi-mc-l_data.req+
                     :dest-address group-address
                     :apci (make-apci-gv-write)
                     :dpt dpt))))
    (! *async-handler* `(:send . ,req))
    req))

(defun send-read-request (group-address)
  "Send a tunnelling-request as L-Data.Req with APCI Group-Value-Read to the given `address:knx-group-address`. The response to this request will be received asynchronously.
Returns the request that was sent."
  (check-type group-address knx-group-address)
  (%assert-channel-id)
  (let ((req (make-tunnelling-request
              :channel-id *channel-id*
              :seq-counter (%next-seq-counter)
              :cemi (make-default-cemi
                     :message-code +cemi-mc-l_data.req+
                     :dest-address group-address
                     :apci (make-apci-gv-read)
                     :dpt nil))))
    (! *async-handler* `(:send . ,req))
    req))

;; ---------------------------------
;; async-handler
;; ---------------------------------

(defun async-handler-receive (msg)
  "Allows the following messages:

- `(:send . <request>)` to send an knx request to the gateway.

- `(:receive . nil)` to start receive knx requests/responses from the gateway. The receival itself is done in a separate task (sento.tasks API). The result of the receival is forwarded to:

- `(:received . <result>)` looks at what is the type of the received.
For `knx-tunnelling-request`s the registered listener functions will be called. All else will be enqueued in the `*awaited-things*` list, for `:wait-on-resp` to check.

- `(:wait-on-resp . (<resp-type> <start-time> <wait-time>))` to wait (by retrying and checking on the enqueued messages, the actor is not blocked) for a response of type `<resp-type>` until the time `<start-time> + <wait-time> (defaults to *resp-wait-timeout-secs*)` has elapsed. If the time has elapsed, a condition of type `knx-receive-error` will be signalled. If a response of the correct type is received, the response will be replied to the sender of the request.

- `(:heartbeat . nil)` to send a connection-state request to the KNXnet/IP gateway."
  (destructuring-bind (msg-sym . args) msg
    (log:trace "Async-handler received msg: ~a" msg-sym)
    (let ((self act:*self*)
          (sender act:*sender*))
      (labels ((timeout-elapsed-p (start-time resp-wait-time)
                 (> (get-universal-time)
                    (+ resp-wait-time start-time)))
               (doasync (dispatcher fun &optional (completed-fun nil))
                 (tasks:with-context ((act:context *async-handler*) dispatcher)
                   (if completed-fun
                       (tasks:task-async fun
                                         :on-complete-fun completed-fun)
                       (tasks:task-start fun))))
               (wait-and-call-again (resp-type start-time resp-wait-time)
                 (doasync :waiter
                          (lambda ()
                            (sleep 0.2)
                            (! self `(:wait-on-resp
                                      . (,resp-type ,start-time ,resp-wait-time))
                               sender)))))
        (case msg-sym
          (:send
           (ip-send-knx-data args))
        
          (:receive
           (doasync :receiver
                    (lambda ()
                      (prog1
                          (handler-case
                              ;; this call blocks until data is available
                              ;; or there is an error
                              (ip-receive-knx-data)
                            (error (c)
                              (log:warn "Error on receiving: ~a" c)))
                        (sleep *receive-knx-data-recur-delay-secs*)
                        (! self `(:receive . nil))))
                    (lambda (result)
                      (handler-case
                          (when (and result (car result))
                            (log:debug "KNX response received: (~a ~a)"
                                       (type-of (first result))
                                       (second result))
                            (! self `(:received . ,result)))
                        (error (c)
                          (log:warn "Error on receiving: ~a" c))))))

          (:received
           (destructuring-bind (received err) args
             (declare (ignore err))
             (let ((received-type (type-of received)))
               (log:debug "Received: ~a" received-type)
               (typecase received
                 (knx-tunnelling-request
                  (progn
                    (log:debug "Notifying listeners of received tunnelling request...")
                    (dolist (listener-fun *tunnel-request-listeners*)
                      (ignore-errors
                       (funcall listener-fun received)))))
                 (knx-disconnect-request
                  (progn
                    (log:info "Received ip-disconnect request.")
                    (setf *channel-id* nil)
                    (setf *seq-counter* 0)))
                 (t
                  (if (null (gethash received-type *awaited-things*))
                      (log:debug "Discarding received: ~a" received-type)
                      (progn
                        (log:debug "Filling awaited response: ~a" received-type)
                        (setf (gethash received-type *awaited-things*) args))))))))
          
          (:wait-on-resp
           (destructuring-bind (resp-type start-time resp-wait-time) args
             (if (null (gethash resp-type *awaited-things*))
                 (setf (gethash resp-type *awaited-things*) 'awaiting))
             (when (timeout-elapsed-p start-time resp-wait-time)
               (log:info "Time elapsed waiting for response of type: ~a" resp-type)
               (reply `(nil ,(make-condition
                              'knx-response-timeout-error
                              :format-control
                              (format nil
                                      "Timeout waiting for response of type ~a"
                                      resp-type))))
               (remhash resp-type *awaited-things*)
               (return-from async-handler-receive))
             (log:trace "Checking for response of type: ~a" resp-type)
             (let ((thing (gethash resp-type *awaited-things*)))
               (if (not (eq thing 'awaiting))
                   (destructuring-bind (response err) thing
                     (log:debug "Received thing: (resp:~a err:~a)" (type-of response) err)
                     (reply thing)
                     (remhash resp-type *awaited-things*))
                   (progn
                     (log:trace "Thing not received yet")
                     (wait-and-call-again resp-type start-time resp-wait-time))))))

          (:heartbeat
           ;; extended wait timeout according to spec
           (let ((*resp-wait-timeout-secs*
                   *heartbeat-resp-wait-timeout-secs*))
             (send-connection-state)))

          (:add-tunnel-req-listener
           (push args *tunnel-request-listeners*))
          (:rem-tunnel-req-listener
           (setf *tunnel-request-listeners*
                 (remove args *tunnel-request-listeners*)))
          (:clr-tunnel-req-listeners
           (setf *tunnel-request-listeners* nil)))))))

(defun start-async-receive ()
  (assert *async-handler* nil "No async-handler set!")
  (! *async-handler* '(:receive . nil)))

(defun start-heartbeat ()
  (assert *async-handler* nil "No async-handler set!")
  (let ((scheduler (asys:scheduler
                    (ac:system
                     (act:context *async-handler*)))))
    (wt:schedule-recurring scheduler
                           *heartbeat-interval-secs*
                           *heartbeat-interval-secs*
                           (lambda ()
                             (! *async-handler* '(:heartbeat . nil))))))

(defun make-async-handler (actor-context)
  (assert (null *async-handler*) nil "Async-handler already set!")
  (setf *async-handler* (ac:actor-of
                         actor-context
                         :name "KNX receiver"
                         :receive (lambda (msg)
                                    (async-handler-receive msg)))))
