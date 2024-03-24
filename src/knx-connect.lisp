(defpackage :knx-conn.knx-connect
  (:use :cl :ip-client :knx-client :sento.future)
  (:nicknames :knxc)
  (:export #:knx-conn-init
           #:knx-conn-destroy
           #:with-knx/ip
           #:write-value))

(in-package :knx-conn.knx-connect)

(defparameter *knx-if* "192.168.50.41")

(defvar *asys* nil
  "The actor system used for async communication.")

;; ---------------------------------
;; actor-system, actors, etc
;; ---------------------------------

(defun %ensure-asys ()
  (log:info "Ensuring actor system...")
  (unless *asys*
    (log:info "Creating actor system...")
    (setf *asys* (asys:make-actor-system '(:dispatchers
                                           (:shared (:workers 2)
                                            :receiver (:workers 1)
                                            :waiter (:workers 1)))))))

(defun %shutdown-asys ()
  (log:info "Shutting down actor system...")
  (when *asys*
    (ac:shutdown *asys* :wait t)
    (setf *asys* nil)))

(defun %register-tunnel-request-listener (listener-fun)
  "Register the given `listener-fun` to be called when a tunnelling request is received.
The function is called with the `knx-tunnelling-request` as argument.
Make sure that the function is not doing lon-running operations or else spawn a new task/thread so that it will not block/delay the receival of further requests."
  (check-type listener-fun function)
  (log:info "Registering listener...")
  (push listener-fun *tunnel-request-listeners*))

;; ---------------------------------
;; top-level functions
;; ---------------------------------

(defun knx-conn-init (host &key (port 3671)
                             (start-receive t)
                             (enable-heartbeat t)
                             (tunnel-request-listeners nil))
  "Initialize and setup the KNX connection and other internal structures."
  (log:info "Initializing KNX...")
  (ip-connect host port)
  (when tunnel-request-listeners
    (dolist (listener-fun tunnel-request-listeners)
      (%register-tunnel-request-listener listener-fun)))
  (%ensure-asys)
  (unless *async-handler*
    (log:info "Creating async-handler...")
    (make-async-handler *asys*))
  (when start-receive
    (log:info "Starting async-receive...")
    (start-async-receive))
  (when enable-heartbeat
    (log:info "Starting heartbeat...")
    (start-heartbeat)))

(defun knx-conn-destroy ()
  "Close the KNX connection and destroy the internal structures."
  (log:info "Destroying KNX...")
  (ignore-errors
   (ip-disconnect))
  (ignore-errors
   (%shutdown-asys))
  (when *async-handler*
    (setf *async-handler* nil))
  (when *tunnel-request-listeners*
    (setf *tunnel-request-listeners* nil)))

;; ---------------------------------
;; convenience functions and macro DSL
;; ---------------------------------

(defun write-value (group-address dpt-type value)
  "Write the given `value` to the `group-address` with the given `dpt-type`."
  (log:info "Writing value: ~a (~a) to ga: ~a" value dpt-type group-address)
  (send-write-request (address:make-group-address group-address)
                      (cond
                        ((eq dpt-type 'dpt:dpt-1.001)
                         (dpt:make-dpt1 dpt-type (if value :on :off))))))

(defmacro with-knx/ip ((host &key (port 3671)) &body body)
  "Macro that initialized and destroys a KNX/IP connection.
Use the `body' to perform operations on the KNX connection, i.e. `write-value'."
  `(unwind-protect
        (progn
          (knx-conn-init ,host :port ,port
                               :start-receive t
                               :enable-heartbeat nil)
          (unless (ip-connected-p)
            (error "Could not connect to KNX/IP"))
          (fawait (establish-tunnel-connection) :timeout 10)
          (unless (tunnel-connection-established-p)
            (error "Could not establish tunnel connection!"))
          ,@body
          (fawait (close-tunnel-connection) :timeout 10))
     (knx-conn-destroy)))
