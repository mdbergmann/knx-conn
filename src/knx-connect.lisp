(defpackage :knx-conn.knx-connect
  (:use :cl :ip-client :knx-client)
  (:nicknames :knxc)
  (:export #:knx-conn-init
           #:knx-conn-destroy
           ))

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
    (setf *tunnel-request-listeners* nil)))

(defun %register-tunnel-request-listener (listener-fun)
  "Register the given `listener-fun` to be called when a tunnelling request is received.
The function is called with the `knx-tunnelling-request` as argument.
Make sure that the function is not doing lon-running operations or else spawn a new task/thread so that it will not block/delay the receival of further requests."
  (check-type listener-fun function)
  (log:info "Registering listener...")
  (push listener-fun *tunnel-request-listeners*))

(defun %make-handler ()
  (unless *async-handler*
    ))

;; ---------------------------------
;; top-level functions
;; ---------------------------------

(defun knx-conn-init (host &key (port 3671)
                             (start-receiving t)
                             (tunnel-request-listeners nil))
  "Initialize and setup the KNX connection and other internal structures."
  (log:info "Initializing KNX...")
  (ip-connect host port)
  (when tunnel-request-listeners
    (dolist (listener-fun tunnel-request-listeners)
      (%register-tunnel-request-listener listener-fun)))
  (%ensure-asys)
  (when start-receiving
    (start-async-receive))
  )

(defun knx-conn-destroy ()
  "Close the KNX connection and destroy the internal structures."
  (log:info "Destroying KNX...")
  (ip-disconnect)
  (when *asys*
    (%shutdown-asys))
  )
