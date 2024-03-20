(defpackage :knx-conn.knx-connect
  (:use :cl :knxutil :knxobj :descr-info :connect :tunnelling
        :hpai :cemi :address :dpt
        :sento.future)
  (:nicknames :knxc)
  (:import-from #:sento.actor
                #:!
                #:?
                #:reply)
  (:export #:knx-conn-init
           #:knx-conn-destroy
           ;; send requests
           #:retrieve-descr-info
           #:establish-tunnel-connection
           #:close-tunnel-connection
           #:send-connection-state
           #:send-write-request
           #:send-read-request
           ;; conditions
           #:knx-receive-error
           ))

(in-package :knx-conn.knx-connect)

(defparameter *knx-if* "192.168.50.41")

;; -----------------------------
;; low-level communication (UDP)
;; -----------------------------

(defvar *conn* nil)

(defun %ip-connect (address &optional (port 3671))
  "Connect to the KNXnet/IP gateway at the given `address` and `port`."
  (assert (null *conn*) nil "Already connected!")
  (let ((conn (usocket:socket-connect
               address port
               :protocol :datagram
               :element-type 'octet)))
    (assert conn nil "Could not %ip-connect to ~a on port ~a" address port)
    (log:info "Connected to ~a on port ~a" address port)
    (setf *conn* conn)))

(defun %ip-disconnect ()
  "Disconnect from the KNXnet/IP gateway."
  (assert *conn* nil "No connection!")
  (usocket:socket-close *conn*)
  (setf *conn* nil))

(defun %ip-send-knx-data (request)
  "Send the given `request` to the KNXnet/IP gateway."
  (assert *conn* nil "No connection!")
  (log:debug "Sending obj: ~a" request)
  (let ((req-bytes (to-byte-seq request)))
    (check-type req-bytes (simple-array (unsigned-byte 8) (*)))
    (log:debug "Sending bytes: ~a" req-bytes)
    (usocket:socket-send *conn* req-bytes (length req-bytes)))
  request)

(defun %ip-receive-knx-data ()
  "Receive a KNXnet/IP request from the KNXnet/IP gateway.
Returns a list of the received object and an error condition, if any."
  (assert *conn* nil "No connection!")
  (log:debug "Receiving data...")
  (let ((buf (make-array 1024 :element-type 'octet)))
    (handler-case 
        (let ((received-obj
                (parse-root-knx-object
                 (usocket:socket-receive *conn* buf 1024))))
          (log:debug "Received obj: ~a" received-obj)
          `(,received-obj nil))
      (error (e)
        (log:info "Error: ~a" e)
        `(nil ,e))
      (condition (c)
        (log:info "Condition: ~a" c)
        `(nil ,c)))))

;; -----------------------------
;; helpers and vars
;; -----------------------------

(defvar *asys* nil
  "The actor system used for async communication.")

(defvar *async-handler* nil
  "The actor that handles the async communication.")

(defvar *resp-wait-timeout-secs* 3
  "Timeout for waiting for a response.")

(defvar *tunnel-request-listeners* nil
  "A list of functions to be called when a tunnelling request is received.")

(defparameter *default-receive-knx-data-recur-delay-secs* 0)
(defvar *receive-knx-data-recur-delay-secs*
  *default-receive-knx-data-recur-delay-secs*
  "Defines a delay in seconds for the recurring retrieval of KNX data.
Only applicable if `start-receiving` is true in `knx-conn-init`.")

(defvar *received-things* nil "Pool of received messages that are not handled otherwise.")

(defvar *channel-id* nil
  "The channel-id of the current tunnelling connection.")

(defvar *seq-counter* 0
  "The sequence counter for the current tunnelling connection.")

(defun %assert-channel-id ()
  (assert (integerp *channel-id*)
          nil "No open connection!"))

(defun %next-seq-counter ()
  (setf *seq-counter* (mod (1+ *seq-counter*) 255)))

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
  (? *async-handler* `(:wait-on-resp-type
                       . (knx-descr-response ,(get-universal-time)))))

(defun establish-tunnel-connection ()
  "Send a tunnelling connection to the KNXnet/IP gateway. The response to this request will be received asynchronously.
Returns a future. The result will be a list of the received response and an error condition, if any.
The error condition will be of type `knx-receive-error` and reflects just an error of transport or parsing. The response itself may contain an error status of the KNX protocol.

If the connection is established successfully, the channel-id will be stored in the global variable `*channel-id*`."
  (log:info "Establishing tunnel connection...")
  (! *async-handler* `(:send . ,(make-connect-request)))
  (let ((fut
          (? *async-handler*
             `(:wait-on-resp-type
               . (knx-connect-response ,(get-universal-time))))))
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
             `(:wait-on-resp-type
               . (knx-disconnect-response ,(get-universal-time))))))
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

(defun send-connection-state ()
  "Sends a connection-state request to the KNXnet/IP gateway. The response to this request will be received asynchronously.
Returns the request that was sent.
This request should be sent every some seconds (i.e. 60) as a heart-beat to keep the connection alive."
  (%assert-channel-id)
  (! *async-handler* `(:send
                       . ,(make-connstate-request *channel-id*)))
  (? *async-handler* `(:wait-on-resp-type
                       . (knx-connstate-response ,(get-universal-time)))))

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
;; actor-system, actors, etc
;; ---------------------------------

(define-condition knx-receive-error (simple-error) ()
  (:report (lambda (c s)
             (format s "KNX receive error: ~a" (simple-condition-format-control c)))))

(defun %ensure-asys ()
  (log:info "Ensuring actor system...")
  (unless *asys*
    (log:info "Creating actor system...")
    (setf *asys* (asys:make-actor-system '(:dispatchers
                                           (:shared (:workers 2)
                                            :receiver (:workers 1)
                                            :waiter (:workers 1))
                                           :scheduler
                                           (:enabled :false))))
    (unless *async-handler*
      (log:info "Creating async-handler...")
      (%make-handler))))

(defun %shutdown-asys ()
  (log:info "Shutting down actor system...")
  (when *asys*
    (ac:shutdown *asys* :wait t)
    (setf *asys* nil))
  (when *async-handler*
    (setf *async-handler* nil))
  (when *tunnel-request-listeners*
    (setf *tunnel-request-listeners* nil))
  (when *received-things*
    (setf *received-things* nil)))

(defun %assert-async-handler ()
  (assert *asys* nil "No actor system!")
  (assert *async-handler* nil "No async-handler!"))

(defun %register-tunnel-request-listener (listener-fun)
  "Register the given `listener-fun` to be called when a tunnelling request is received.
The function is called with the `knx-tunnelling-request` as argument.
Make sure that the function is not doing lon-running operations or else spawn a new task/thread so that it will not block/delay the receival of further requests."
  (check-type listener-fun function)
  (log:info "Registering listener...")
  (push listener-fun *tunnel-request-listeners*))

;; ---------------------------------
;; async-handler
;; ---------------------------------

(defun %handler-receive (msg)
  "Allows the folloing messages:

- `(:send . <request>)` to send an knx request to the gateway.

- `(:receive . nil)` to start receive knx requests/responses from the gateway. The receival itself is done in a separate task (sento.tasks API). The result of the receival is forwarded to:

- `(:received . <result>)` looks at what is the type of the received.
For `knx-tunnelling-request`s the registered listener functions will be called. All else will be enqueued in the `*received-things*` list, for `:wait-on-resp-type` to check.

- `(:wait-on-resp-type . (<resp-type> <start-time>))` to wait (by retrying and checking on the enqueued messages, the actor is not blocked) for a response of type `<resp-type>` until the time `<start-time> + *resp-wait-timeout-secs*` has elapsed. If the time has elapsed, a condition of type `knx-receive-error` will be signalled. If a response of the correct type is received, the response will be replied to the sender of the request."
  (destructuring-bind (msg-sym . args) msg
    (log:debug "Async-handler received msg: ~a" msg-sym)
    (let ((self act:*self*)
          (sender act:*sender*))
      (labels ((timeout-elapsed-p (start-time)
                 (> (get-universal-time)
                    (+ *resp-wait-timeout-secs* start-time)))
               (doasync (dispatcher fun &optional (completed-fun nil))
                 (tasks:with-context (*asys* dispatcher)
                   (if completed-fun
                       (tasks:task-async fun
                                         :on-complete-fun completed-fun)
                       (tasks:task-start fun))))
               (wait-and-call-again (resp-type start-time)
                 (doasync :waiter
                          (lambda ()
                            (sleep 0.2)
                            (! self `(:wait-on-resp-type
                                      . (,resp-type ,start-time))
                               sender)))))
        (case msg-sym
          (:send
           (%ip-send-knx-data args))
        
          (:receive
           (doasync :receiver
                    (lambda ()
                      (prog1
                          (handler-case
                              ;; this call blocks until data is available
                              ;; or there is an error
                              (%ip-receive-knx-data)
                            (error (c)
                              (log:warn "Error on receiving: ~a" c)))
                        (sleep *receive-knx-data-recur-delay-secs*)
                        (! self `(:receive . nil))))
                    (lambda (result)
                      (when (and result (car result))
                        (log:debug "KNX response received: (~a ~a)"
                                   (type-of (first result))
                                   (second result))
                        (! self `(:received . ,result))))))

          (:received
           (destructuring-bind (received err) args
             (declare (ignore err))
             (log:debug "Received: ~a" (type-of received))
             (typecase received
               (knx-tunnelling-request
                (progn
                  (log:debug "Notifying listeners of received tunnelling request...")
                  (dolist (listener-fun *tunnel-request-listeners*)
                    (funcall listener-fun received))))
               (knx-disconnect-request
                (progn
                  (log:info "Received %ip-disconnect request.")
                  (setf *channel-id* nil)
                  (setf *seq-counter* 0)))
               (t
                (progn
                  (log:debug "Enqueuing: ~a" received)
                  (push args *received-things*))))))
          
          (:wait-on-resp-type
           (destructuring-bind (resp-type start-time) args
             (when (timeout-elapsed-p start-time)
               (log:debug "Time elapsed waiting for response of type: ~a" resp-type)
               (reply `(nil ,(make-condition
                              'knx-receive-error
                              :format-control
                              (format nil
                                      "Timeout waiting for response of type ~a"
                                      resp-type))))
               (return-from %handler-receive))
             (log:trace "Checking for response of type: ~a" resp-type)
             (loop
               (let ((thing (pop *received-things*)))
                 (unless thing
                   (log:trace "No thing received yet")
                   (wait-and-call-again resp-type start-time)
                   (return))
                 (destructuring-bind (response err) thing
                   (log:debug "Popped thing: (resp:~a err:~a)" (type-of response) err)
                   (if (typep response resp-type)
                       (reply thing)
                       (wait-and-call-again resp-type start-time))))))))))))

(defun %make-handler ()
  (unless *async-handler*
    (setf *async-handler* (ac:actor-of
                           *asys*
                           :name "KNX receiver"
                           :receive (lambda (msg)
                                      (%handler-receive msg))))))

(defun %start-async-receiving ()
  (%assert-async-handler)
  (! *async-handler* '(:receive . nil)))

;; ---------------------------------
;; top-level functions
;; ---------------------------------

(defun knx-conn-init (host &key (port 3671)
                             (start-receiving t)
                             (tunnel-request-listeners nil))
  "Initialize and setup the KNX connection and other internal structures."
  (log:info "Initializing KNX...")
  (%ip-connect host port)
  (when tunnel-request-listeners
    (dolist (listener-fun tunnel-request-listeners)
      (%register-tunnel-request-listener listener-fun)))
  (%ensure-asys)
  (when start-receiving
    (%start-async-receiving))
  )

(defun knx-conn-destroy ()
  "Close the KNX connection and destroy the internal structures."
  (log:info "Destroying KNX...")
  (when *conn*
    (%ip-disconnect))
  (when *asys*
    (%shutdown-asys))
  )
