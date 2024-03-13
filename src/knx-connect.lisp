(defpackage :knx-conn.knx-connect
  (:use :cl :knxutil :knxobj :descr-info :connect :tunnelling
        :hpai :cemi :address :dpt)
  (:nicknames :knxc)
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
           ))

(in-package :knx-conn.knx-connect)

(defparameter *knx-if* "192.168.50.41")

(defvar *conn* nil)

(defun connect (address &optional (port 3671))
  "Connect to the KNXnet/IP gateway at the given `address` and `port`."
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
  "Receive a KNXnet/IP request from the KNXnet/IP gateway."
  (assert *conn* nil "No connection!")
  (let ((buf (make-array 1024 :element-type 'octet)))
    (log:debug "Receiving data...")
    (handler-case 
        (let ((received-obj
                (parse-root-knx-object
                 (usocket:socket-receive *conn* buf 1024))))
          (log:debug "Received obj: ~a" received-obj)
          (values received-obj nil))
      (error (e)
        (log:info "Error: ~a" e)
        (values nil e))
      (condition (c)
        (log:info "Condition: ~a" c)
        (values nil c)))))

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

(defvar *asys* nil)
(defvar *sender* nil "actor")
(defvar *receiver* nil "actor")

(defun %ensure-asys ()
  (log:info "Ensuring actor system...")
  (unless *asys*
    (setf *asys* (asys:make-actor-system '(:dispatchers
                                           (:shared (:workers 2)
                                            :receiver (:workers 1)
                                            :waiter (:workers 1))
                                           :scheduler
                                           (:enabled :false)))))
  (%make-sender)
  (%make-receiver))

(defun %shutdown-asys ()
  (log:info "Shutting down actor system...")
  (when *asys*
    (ac:shutdown *asys* :wait t)
    (setf *asys* nil)
    (setf *sender* nil)
    (setf *receiver* nil)))

;; sender

(defun %sender-receive (msg)
  "`MSG` is request"
  (log:debug "Sender received: ~a" msg)
  (send-knx-data msg))

(defun %make-sender ()
  (unless *sender*
    (setf *sender* (ac:actor-of *asys*
                                :name "KNX sender"
                                :receive (lambda (msg) (%sender-receive msg))))))

;; receiver

(defvar *received-things* nil)

(defun %receiver-receive (msg)
  (log:debug "Receiver received: ~a" msg)
  (let ((self act:*self*)
        (sender act:*sender*))
    (case (car msg)
      (:receive (tasks:with-context (*asys* :receiver)
                  (tasks:task-async
                   (lambda ()
                     (receive-knx-data))
                   :on-complete-fun
                   (lambda (result)
                     (log:debug "KNX response received: ~a" result)
                     ;; TODO: error handling for `(cons :handler-error foo)`
                     (act:! self `(:enqueue . ,result))))))
      (:enqueue (push (cdr msg) *received-things*))
      (:wait-on-resp-type
       (let ((resp-type (cdr msg)))
         (log:debug "Checking for response of type: ~a" resp-type)
         (flet ((wait-and-call-again ()
                    (tasks:with-context (*asys* :waiter)
                      (tasks:task-start
                       (lambda ()
                         (sleep 0.2)
                         (act:! self `(:wait-on-resp-type . ,resp-type) sender))))))
           (loop
             (let ((thing (pop *received-things*)))
               (unless thing
                 (log:debug "No thing received yet")
                 (wait-and-call-again)
                 (return))
               (log:debug "Popped thing of type: ~a" (type-of thing))
               (if (typep thing resp-type)
                   (progn
                     (log:debug "Replying to caller.")
                     (act:reply thing))
                   (wait-and-call-again))))))))))

(defun %make-receiver ()
  (unless *receiver*
    (setf *receiver* (ac:actor-of *asys*
                                  :name "KNX receiver"
                                  :dispatcher :pinned
                                  :receive (lambda (msg)
                                             (%receiver-receive msg))))
    (act:! *receiver* '(:receive . nil))))

;; ---------------------------------

(defmacro %with-request-response (request)
  `(progn
     (send-knx-data ,request)
     (receive-knx-data)))

(defun retrieve-descr-info ()
  (let ((req (make-descr-request *hpai-unbound-addr*)))
    (act:! *sender* req)
    (act:? *receiver* `(:wait-on-resp-type . knx-descr-response))))
  
(defun establish-tunnel-connection ()
  (multiple-value-bind (response err)
      (%with-request-response (make-connect-request))
    (when response
      (setf *channel-id*
            (connect-response-channel-id response)))
    (values response err)))

(defun close-tunnel-connection ()
  (%assert-channel-id)
  (%with-request-response (make-disconnect-request *channel-id*)))

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
