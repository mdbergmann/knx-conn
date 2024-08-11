(defpackage :knx-conn.knx-client
  (:use :cl :ip-client :knxobj :hpai :dpt :cemi :address
   :descr-info :connect :tunnelling
        :sento.future)
  (:nicknames :knx-client)
  (:import-from #:sento.actor
                #:!
                #:?
                #:reply)
  (:import-from #:timeutils
                #:wait-cond)
  (:shadowing-import-from #:alexandria
                          #:when-let
                          #:when-let*)
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
           #:*response-wait-timeout-secs*
           #:*group-address-dpt-mapping*
           #:reset-client-vars
           ;; async handler
           #:*async-handler*
           #:async-handler-receive
           #:start-async-receive
           #:make-async-handler
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
             (format s "KNX receive error: ~a"
                     (simple-condition-format-control c)))))

(define-condition knx-response-timeout-error (simple-error) ()
  (:report (lambda (c s)
             (format s "KNX timeout error: ~a"
                     (simple-condition-format-control c)))))

;; ---------------------------------
;; configuration
;; ---------------------------------

;; response timeouts

(defparameter *default-response-wait-timeout-secs* 3
  "Default timeout for waiting for a response.
Times must be in full seconds. Float values will be truncated.")

(defparameter *tunnel-ack-wait-timeout-secs* 2
  "Timeout for waiting for a tunnelling ack response. Should be 1, we set 2
Times must be in full seconds. Float values will be truncated.")

(defconstant +heartbeat-resp-wait-timeout-secs+ 10
  "Timeout for waiting for a heartbeat response.")

(defparameter *response-wait-timeout-secs* *default-response-wait-timeout-secs*
  "Timeout for waiting for a response.
Times must be in full seconds. Float values will be truncated.")

;; recur delay -- private

(defparameter *default-receive-knx-data-recur-delay-secs* 0
  "Default delay in seconds for the recurring retrieval of KNX data.
The default is 0 because there should be no delay.")

(defparameter *receive-knx-data-recur-delay-secs*
  *default-receive-knx-data-recur-delay-secs*
  "Defines a delay in seconds for the recurring retrieval of KNX data.
Only applicable if `start-receive` is true in `knx-conn-init`.")

;; heartbeat

(defparameter *default-heartbeat-interval-secs* 60
  "Default interval in seconds for sending a connection-state request to the KNXnet/IP gateway.")

(defparameter *heartbeat-interval-secs* *default-heartbeat-interval-secs*
  "Interval in seconds for sending a connection-state request to the KNXnet/IP gateway.")

;; ---------------------------------
;; global variables
;; ---------------------------------

;; public
;; --------------------

(defvar *group-address-dpt-mapping* nil
  "A mapping of group-addresses to their data point types.
The mapping is a list of lists where each list has the form (group-address dpt-type label).
The group-address is a string representation of the address, i.e. \"1/1/1\".
The dpt-type is the type as available in the `dpt` package, i.e.: `dpt:dpt-1.001`.
The label is a string that describes the group-address and is used for logging purposes.")

;; private
;; --------------------

(defvar *channel-id* nil
  "The channel-id of the current tunnelling connection.")

(defvar *seq-counter* 0
  "The sequence counter for the current tunnelling connection.
It is imperative that the seq-counter starts with 0 on every new connection.")

(defvar *tunnel-request-listeners* nil
  "A list of functions to be called when a tunnelling request is received.")

(defvar *awaited-things* (make-hash-table :test #'eq)
  "Pool of received messages that are not handled otherwise.")

(defvar *heartbeat-timer-sig* nil
  "The signature of the heartbeat timer.")

;; ----------- helper functions ------------

(defun reset-client-vars ()
  (setf *receive-knx-data-recur-delay-secs*
        *default-receive-knx-data-recur-delay-secs*)
  (setf *heartbeat-interval-secs*
        *default-heartbeat-interval-secs*)
  (setf *response-wait-timeout-secs*
        *default-response-wait-timeout-secs*)
  (setf *group-address-dpt-mapping* nil)
  (setf *awaited-things*
        (make-hash-table :test #'eq))
  t)

(defun %assert-channel-id ()
  (assert (integerp *channel-id*)
          nil "No open connection!"))

(defun %next-seq-counter ()
  (prog1
      *seq-counter*
    (setf *seq-counter* (mod (1+ *seq-counter*) 255))))

(defun %start-heartbeat ()
  (assert *async-handler* nil "No async-handler set!")
  (let ((scheduler (asys:scheduler
                    (ac:system
                     (act:context *async-handler*)))))
    (setf *heartbeat-timer-sig*
          (wt:schedule-recurring scheduler
                                 *heartbeat-interval-secs*
                                 *heartbeat-interval-secs*
                                 (lambda ()
                                   (! *async-handler* '(:heartbeat . nil)))))))

(defun %stop-heartbeat ()
  (assert *async-handler* nil "No async-handler set!")
  (when *heartbeat-timer-sig*
    (let ((scheduler (asys:scheduler
                      (ac:system
                       (act:context *async-handler*)))))
      (wt:cancel scheduler *heartbeat-timer-sig*))))

;; request listeners can be added during runtime
;; so we have to take care of thread-safety

(defun add-tunnelling-request-listener (listener-fun)
  (! *async-handler* `(:add-tunnel-req-listener . ,listener-fun)))

(defun rem-tunnelling-request-listener (listener-fun)
  (! *async-handler* `(:rem-tunnel-req-listener . ,listener-fun)))

(defun clr-tunnelling-request-listeners ()
  (! *async-handler* '(:clr-tunnel-req-listeners)))

;; ---------------------------------
;; knx-ip protocol functions
;; ---------------------------------

(defun %send-req (req)
  (! *async-handler* `(:send . ,req)))

(defun %receive-resp (resp-type &optional (resp-wait-time
                                           *response-wait-timeout-secs*))
  (? *async-handler* `(:wait-on-resp
                       . (,resp-type
                          ,(get-universal-time)
                          ,resp-wait-time))))

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
  (%send-req (make-descr-request *hpai-unbound-addr*))
  (%receive-resp 'knx-descr-response))

(defun send-connection-state ()
  "Sends a connection-state request to the KNXnet/IP gateway. The response to this request will be received asynchronously.
Returns the request that was sent.
This request should be sent every some seconds (i.e. 60) as a heart-beat to keep the connection alive."
  (%assert-channel-id)
  (%send-req (make-connstate-request
              *channel-id*
              ip-client:*local-host-and-port*))
  (%receive-resp 'knx-connstate-response))

(defun establish-tunnel-connection (&optional (enable-heartbeat t))
  "Send a tunnelling connection to the KNXnet/IP gateway. The response to this request will be received asynchronously.
Returns a future. The result will be a list of the received response and an error condition, if any.
The error condition will be of type `knx-receive-error` and reflects just an error of transport or parsing. The response itself may contain an error status of the KNX protocol.

If the connection is established successfully, the channel-id will be stored in the global variable `*channel-id*`."
  (log:info "Establishing tunnel connection...")
  (%send-req (make-connect-request
              ip-client:*local-host-and-port*
              ip-client:*local-host-and-port*))
  (%handle-response-fut
   (%receive-resp 'knx-connect-response)
   (lambda (response)
     (let ((status (connect-response-status response)))
       (if (not (eql status 0))
           (log:warn "Tunnel connection failed, status: ~a" status)
           (progn
             (log:info "Tunnel connection established.")
             (log:info "Channel-id: ~a" (connect-response-channel-id response))
             (setf *channel-id*
                   (connect-response-channel-id response))
             (setf *seq-counter* 0)
             (when enable-heartbeat
               (log:info "Starting heartbeat...")
               (%start-heartbeat))))))))

(defun close-tunnel-connection ()
  (%assert-channel-id)
  (%stop-heartbeat)
  (log:info "Closing tunnel connection...")
  (%send-req (make-disconnect-request
              *channel-id*
              ip-client:*local-host-and-port*))
  (%handle-response-fut
   (%receive-resp 'knx-disconnect-response)
   (lambda (response)
     (let ((status (disconnect-response-status response)))
       (if (not (eql status 0))
           (log:warn "Tunnel disconnection failed, status: ~a" status)
           (progn
             (log:info "Tunnel connection closed.")
             (setf *channel-id* nil)))))))

(defun tunnel-connection-established-p ()
  "Returns `T' if a tunnel connection is established."
  (integerp *channel-id*))

;; ---------------------------------
;; tunnelling-request functions
;; ---------------------------------

(defun %send-tunnel-request (req)
  "Sends given tunnel request and waits for tunnel-ack, resends request once if first ack doesn't come in time."
  (log:trace "Checking on no awaited ACK...")
  (unless (wait-cond
           (lambda ()
             (null (gethash 'knx-tunnelling-ack *awaited-things*)))
           0.05 5) ;; wait for max 5 seconds => too long
    (log:warn "Unable to send request, ACK still being awaited!")
    (return-from %send-tunnel-request))
  
  (log:trace "Check OK, go on...")
  (let ((ack-timeout *tunnel-ack-wait-timeout-secs*))
    (%send-req req)
    (fmap (%receive-resp 'knx-tunnelling-ack ack-timeout)
        (result)
      (destructuring-bind (resp err) result
        (declare (ignore resp))
        (if (and err (typep err 'knx-response-timeout-error))
            (progn
              (log:debug "Received no ACK in time, resending request.")
              (%send-req req)
              (%receive-resp 'knx-tunnelling-ack ack-timeout))
            result)))))

(defun send-write-request (group-address dpt)
  "Send a tunnelling-request as L-Data.Req with APCI Group-Value-Write to the given `address:knx-group-address` with the given data point type to be set.
Returns a `fcomputation:future` that is resolved with the tunnelling-ack when received."
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
    (%send-tunnel-request req)))

(defun send-read-request (group-address)
  "Send a tunnelling-request as L-Data.Req with APCI Group-Value-Read to the given `address:knx-group-address`. The response to this request will be received asynchronously.
Returns a `fcomputation:future` that is resolved with the tunnelling-ack when received."
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
    (%send-tunnel-request req)))

;; ---------------------------------
;; async-handler
;; ---------------------------------

(defun %doasync (dispatcher fun &optional (completed-fun nil))
  (tasks:with-context ((act:context *async-handler*) dispatcher)
    (if completed-fun
        (tasks:task-async fun
                          :on-complete-fun completed-fun)
        (tasks:task-start fun))))

(defun %async-handler-knx-send (knxobj)
  (ip-send-knx-data knxobj))

(defun %async-handler-knx-receive (self)
  (%doasync :receiver
            (lambda ()
              (prog1
                  (handler-case
                      ;; this call blocks until data is available
                      ;; or there is an error
                      (ip-receive-knx-data)
                    (error (c)
                      (log:warn "Error on receiving: ~a" c)))
                ;; in tests only to slow down the loop
                (sleep *receive-knx-data-recur-delay-secs*)
                (when (ip-connected-p)
                  (! self `(:receive . nil)))))
            (lambda (result)
              (handler-case
                  (when (and result (car result))
                    (log:debug "KNX message received: (~a ~a)"
                               (type-of (first result))
                               (second result))
                    (! self `(:received . ,result)))
                (error (c)
                  (log:warn "Error on received: ~a" c))))))

(defun %handle-tunnelling-request (self received)
  (! self `(:send . ,(make-tunnelling-ack received)))
  (let ((msc (tunnelling-cemi-message-code received)))
    (log:debug "Tunnelling request, msg-code: ~a"
               (cemi-mc-l_data-rep msc))
    (cond
      ((eql msc +cemi-mc-l_data.ind+)
       (let* ((cemi (tunnelling-request-cemi received))
              (addr-src (cemi-source-addr cemi))
              (addr-src-string (address-string-rep addr-src))
              (ga-dest (cemi-destination-addr cemi))
              (ga-dest-string (address-string-rep ga-dest))
              (cemi-data (cemi-data cemi)))
         (log:debug "Tunnelling ind 1: ~a -> ~a = ~a"
                    addr-src-string ga-dest-string cemi-data)
         (when-let* ((mapping-data *group-address-dpt-mapping*)
                     (cemi-data-bytes (when (arrayp cemi-data) cemi-data))
                     (dpt-mapping (find
                                   ga-dest-string mapping-data
                                   :key #'car
                                   :test #'equal)))
           (log:debug "Found mapping for GA: ~a" dpt-mapping)
           (destructuring-bind (ga dpt-type label) dpt-mapping
             (declare (ignore ga))
             (let ((dpt (dpt:parse-to-dpt dpt-type cemi-data)))
               (setf cemi-data dpt)
               (setf (cemi-data cemi) dpt))
             (log:info "Tunnelling ind 2: ~a -> ~a = ~a (~a)"
                       addr-src-string ga-dest-string cemi-data label)))))
      ((eql msc +cemi-mc-l_data.con+)
       (log:debug "Tunnelling confirmation.")))
    (progn
      (log:debug "Notifying listeners of generic L_Data request...")
      (dolist (listener-fun *tunnel-request-listeners*)
        (ignore-errors
         (funcall listener-fun received))))))

(defun %async-handler-knx-received (self received-knxobj)
  (destructuring-bind (received err) received-knxobj
    (declare (ignore err))
    (let ((received-type (type-of received)))
      (log:debug "Received: ~a" received-type)
      (typecase received
        (knx-tunnelling-request
         (%handle-tunnelling-request self received))
        (knx-tunnelling-ack
         (log:debug "Received tunnelling ack: ~a" received))
        (knx-disconnect-request
         (setf *channel-id* nil)
         (setf *seq-counter* 0))
        (knx-connect-response
         (log:debug "Received connect response: ~a" received)))
      (if (null (gethash received-type *awaited-things*))
          (log:debug "Discarding received: ~a" received-type)
          (progn
            (log:debug "Filling awaited response: ~a" received-type)
            (setf (gethash received-type *awaited-things*) received-knxobj))))))

(defun %async-handler-knx-wait (self sender wait-args)
  (labels ((timeout-elapsed-p (start-time resp-wait-time)
             (let ((now (get-universal-time))
                   ;; `resp-wait-time', if float will cause problems in the comparison here
                   (end-time (+ (truncate resp-wait-time) start-time)))
               (> now end-time)))
           (wait-and-call-again (resp-type start-time resp-wait-time)
             (%doasync :waiter
                       (lambda ()
                         (sleep 0.2) ;deliberate wait or the loop will be too fast
                         (! self `(:wait-on-resp
                                   . (,resp-type ,start-time ,resp-wait-time))
                            sender)))))
    (destructuring-bind (resp-type start-time resp-wait-time) wait-args
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
        (return-from %async-handler-knx-wait))
      (log:trace "Checking for awaited response of type: ~a" resp-type)
      (let ((thing (gethash resp-type *awaited-things*)))
        (case thing
          ('awaiting
           (progn
             (log:trace "Awaited thing not received yet")
             (wait-and-call-again resp-type start-time resp-wait-time)))
          (otherwise
           (destructuring-bind (response err) thing
             (log:debug "Received awaited thing: (resp:~a err:~a)" (type-of response) err)
             (reply thing)
             (remhash resp-type *awaited-things*))))))))

(defun %async-handler-knx-heartbeat ()
  ;; extended wait timeout according to spec
  (let ((*response-wait-timeout-secs*
          +heartbeat-resp-wait-timeout-secs+))
    (send-connection-state)))

(defun %async-handler-receive (msg)
  "Allows the following messages:

- `(:send . <request>)` to send an knx request to the gateway.
- `(:receive . nil)` to start receive knx requests/responses from the gateway. The receival itself is done in a separate task (sento.tasks API). The result of the receival is forwarded to:
- `(:received . <result>)` looks at what is the type of the received.
For `knx-tunnelling-request`s the registered listener functions will be called. All else will be enqueued in the `*awaited-things*` list, for `:wait-on-resp` to check.
- `(:wait-on-resp . (<resp-type> <start-time> <wait-time>))` to wait (by retrying and checking on the enqueued messages, the actor is not blocked) for a response of type `<resp-type>` until the time `<start-time> + <wait-time> (defaults to *resp-wait-timeout-secs*, can be dynamically overridden by `let')` has elapsed. If the time has elapsed, a condition of type `knx-receive-error` will be signalled. If a response of the correct type is received, the response will be replied to the sender of the request.
The awaiting of responses works on the communication layer of knx-ip, i.e. search request response, description response/request, connection state request/response, (dis)connect request/response, plus tunnelling request/ack (because those are also quite high-level and can be distinguished on a message type).
Waiting on responses for specific tunnelling requests on an L_Data level must be done via tunnelling listeners.
- `(:heartbeat . nil)` to send a connection-state request to the KNXnet/IP gateway.
- `(:add-tunnel-req-listener . <listener-fun>)` to add a listener function for tunnelling requests.
- `(:rem-tunnel-req-listener . <listener-fun>)` to remove a listener function for tunnelling requests.
- `(:clr-tunnel-req-listeners . nil)` to clear all listener functions for tunnelling requests."
  (destructuring-bind (msg-sym . args) msg
    (log:trace "async-handler received msg: ~a" msg-sym)
    (let ((self act:*self*)
          (sender act:*sender*))
      (case msg-sym
        (:send
         (%async-handler-knx-send args))
        (:receive
         (%async-handler-knx-receive self))
        (:received
         (%async-handler-knx-received self args))          
        (:wait-on-resp
         (%async-handler-knx-wait self sender args))
        (:heartbeat
         (%async-handler-knx-heartbeat))
        ;; tunnel-req-listener management
        (:add-tunnel-req-listener
         (pushnew args *tunnel-request-listeners* :test #'eq))
        (:rem-tunnel-req-listener
         (setf *tunnel-request-listeners*
               (remove args *tunnel-request-listeners*)))
        (:clr-tunnel-req-listeners
         (setf *tunnel-request-listeners* nil))))))

(defun start-async-receive ()
  (assert *async-handler* nil "No async-handler set!")
  (! *async-handler* '(:receive . nil)))

(defun make-async-handler (actor-context)
  (assert (null *async-handler*) nil "Async-handler already set!")
  (setf *async-handler* (ac:actor-of
                         actor-context
                         :name "KNX async handler"
                         :receive (lambda (msg)
                                    (%async-handler-receive msg)))))
