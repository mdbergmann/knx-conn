(defpackage :knx-conn.knx-connect
  (:use :cl :knxutil :knxobj :descr-info :connect :tunnelling
        :hpai :cemi :address :dpt :future)
  (:nicknames :knxc)
  (:import-from #:sento.actor
                #:!
                #:?
                #:reply)
  (:export #:connect
           #:disconnect
           ;; send requests
           #:retrieve-descr-info
           #:establish-tunnel-connection
           #:close-tunnel-connection
           #:send-write-request
           #:send-read-request
           ;; receive data
           #:receive-knx-data
           ;; conditions
           #:knx-receive-error
           ))

(in-package :knx-conn.knx-connect)

(defparameter *knx-if* "192.168.50.41")

(defvar *conn* nil)

(defun connect (address &optional (port 3671))
  "Connect to the KNXnet/IP gateway at the given `address` and `port`."
  (assert (null *conn*) nil "Already connected!")
  (let ((conn (usocket:socket-connect
               address port
               :protocol :datagram
               :element-type 'octet)))
    (assert conn nil "Could not connect to ~a on port ~a" address port)
    (log:info "Connected to ~a on port ~a" address port)
    (setf *conn* conn)))

(defun disconnect ()
  "Disconnect from the KNXnet/IP gateway."
  (assert *conn* nil "No connection!")
  (usocket:socket-close *conn*)
  (setf *conn* nil))

(defun send-knx-data (request)
  "Send the given `request` to the KNXnet/IP gateway."
  (assert *conn* nil "No connection!")
  (log:debug "Sending obj: ~a" request)
  (let ((req-bytes (to-byte-seq request)))
    (check-type req-bytes (simple-array (unsigned-byte 8) (*)))
    (log:debug "Sending bytes: ~a" req-bytes)
    (usocket:socket-send *conn* req-bytes (length req-bytes)))
  request)

(defun receive-knx-data ()
  "Receive a KNXnet/IP request from the KNXnet/IP gateway.
Returns a list of the received object and an error condition, if any."
  (assert *conn* nil "No connection!")
  (let ((buf (make-array 1024 :element-type 'octet)))
    (log:debug "Receiving data...")
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
;; high-level comm
;; -----------------------------

(defvar *channel-id* nil
  "The channel-id of the current tunnelling connection.")

(defun %assert-channel-id ()
  (assert (integerp *channel-id*)
          nil "No open connection!"))

(defvar *seq-counter* 0
  "The sequence counter for the current tunnelling connection.")

(defun %next-seq-counter ()
  (setf *seq-counter* (mod (1+ *seq-counter*) 255)))


;; ---------------------------------
;; comunication queues, actors, etc
;; ---------------------------------

(define-condition knx-receive-error (simple-error) ()
  (:report (lambda (c s)
             (format s "KNX receive error: ~a" (simple-condition-format-control c)))))

(defvar *asys* nil)
(defvar *async-handler* nil "actor")

(defun %ensure-asys ()
  (log:info "Ensuring actor system...")
  (unless *asys*
    (setf *asys* (asys:make-actor-system '(:dispatchers
                                           (:shared (:workers 2)
                                            :receiver (:workers 1)
                                            :waiter (:workers 1))
                                           :scheduler
                                           (:enabled :false)))))
  (%make-handler))

(defun %shutdown-asys ()
  (log:info "Shutting down actor system...")
  (when *asys*
    (ac:shutdown *asys* :wait t)
    (setf *asys* nil)
    (setf *async-handler* nil)))

;; async-handler

(defvar *resp-wait-timeout-secs* 3 "Timeout for waiting for a response.")
(defvar *received-things* nil)

(defun %handler-receive (msg)
  "Allows the folloing messages:

- `(:send . <request>)` to send a request.

- `(:receive . nil)` to start receiving. The receival itself is done in a separate task. The result of the receival will be enqueued by:

- `(:enqueue . <result>)` to enqueue the result of the receival.

- `(:wait-on-resp-type . (<resp-type> <start-time>))` to wait (by retrying and checking on the enqueued messages, the actor is not blocked) for a response of type `<resp-type>` until the time `<start-time> + *resp-wait-timeout-secs*` has elapsed. If the time has elapsed, a condition of type `knx-receive-error` will be signalled. If a response of the correct type is received, the response will be replied to the sender of the request."
  (destructuring-bind (msg-sym . args) msg
    (log:debug "Receiver received: ~a" msg-sym)
    (let ((self act:*self*)
          (sender act:*sender*))
      (flet ((timeout-elapsed-p (start-time)
               (> (get-universal-time)
                  (+ *resp-wait-timeout-secs* start-time)))
             (wait-and-call-again (resp-type start-time)
               (tasks:with-context (*asys* :waiter)
                 (tasks:task-start
                  (lambda ()
                    (sleep 0.2)
                    (! self `(:wait-on-resp-type . (,resp-type ,start-time)) sender))))))
        (case msg-sym
          (:send
           (send-knx-data args))
        
          (:receive
           (tasks:with-context (*asys* :receiver)
             (tasks:task-async
              (lambda ()
                (receive-knx-data))
              :on-complete-fun
              (lambda (result)
                (log:debug "KNX response received: (~a ~a)"
                           (type-of (first result))
                           (second result))
                (! self `(:enqueue . ,result))))))
        
          (:enqueue
           (push args *received-things*))
        
          (:wait-on-resp-type
           (destructuring-bind (resp-type start-time) args
             (when (timeout-elapsed-p start-time)
               (log:debug "Time elapsed waiting for response of type: ~a" resp-type)
               (reply `(nil (make-condition
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
                                      (%handler-receive msg))
                           :init (lambda (self)
                                   (! self '(:receive . nil)))))))

;; ---------------------------------

(defmacro %with-request-response (request)
  `(progn
     (send-knx-data ,request)
     (receive-knx-data)))

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
    (fcompleted fut
        (result)
      (destructuring-bind (response _err) result
        (declare (ignore _err))
        (when response
          (let ((status (connect-response-status response)))
            (if (not (eql status 0))
                (log:warn "Tunnel connection failed, status: ~a" status)
                (progn
                  (log:info "Tunnel connection established.")
                  (log:info "Channel-id: ~a" (connect-response-channel-id response))
                  (setf *channel-id*
                        (connect-response-channel-id response))))))
        (unless response
          (log:warn "No response received."))))
    fut))

(defun close-tunnel-connection ()
  (%assert-channel-id)
  (log:info "Closing tunnel connection...")
  (! *async-handler* `(:send . ,(make-disconnect-request *channel-id*)))
  
  (let ((fut
          (? *async-handler*
             `(:wait-on-resp-type
               . (knx-disconnect-response ,(get-universal-time))))))
    (fcompleted fut
        (result)
      (destructuring-bind (response _err) result
        (declare (ignore _err))
        (when response
          (let ((status (disconnect-response-status response)))
            (if (not (eql status 0))
                (log:warn "Tunnel disconnection failed, status: ~a" status)
                (progn
                  (log:info "Tunnel connection closed.")
                  (setf *channel-id* nil)))))))
    fut))

;; ---------------------------------
;; with opened tunnelling connection
;; ---------------------------------

(defun send-write-request (group-address dpt)
  "Send a tunnelling-request as L-Data.Req with APCI Group-Value-Write to the given `address:knx-group-address` with the given data point type to be set."
  (check-type group-address knx-group-address)
  (check-type dpt dpt)
  (%assert-channel-id)
  (send-knx-data
   (make-tunnelling-request
    :channel-id *channel-id*
    :seq-counter (%next-seq-counter)
    :cemi (make-default-cemi
           :message-code +cemi-mc-l_data.req+
           :dest-address group-address
           :apci (make-apci-gv-write)
           :dpt dpt))))

(defun send-read-request (group-address)
  "Send a tunnelling-request as L-Data.Req with APCI Group-Value-Read to the given `address:knx-group-address`. The response to this request will be received asynchronously."
  (check-type group-address knx-group-address)
  (%assert-channel-id)
  (send-knx-data
   (make-tunnelling-request
    :channel-id *channel-id*
    :seq-counter (%next-seq-counter)
    :cemi (make-default-cemi
           :message-code +cemi-mc-l_data.req+
           :dest-address group-address
           :apci (make-apci-gv-read)
           :dpt nil))))
