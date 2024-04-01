(defpackage :knx-conn.knx-connect
  (:use :cl :ip-client :knx-client :knxobj :cemi :tunnelling :connect :address :dpt
   :sento.future)
  (:nicknames :knxc)
  (:export #:knx-conn-init
           #:knx-conn-destroy
           #:with-knx/ip
           #:write-value
           #:request-value))

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
                                            :receiver-ctrl (:workers 1)
                                            :receiver-data (:workers 1)
                                            :waiter (:workers 1)
                                            :read-request (:workers 1)))))))

(defun %shutdown-asys ()
  (log:info "Shutting down actor system...")
  (when *asys*
    (ac:shutdown *asys* :wait t)
    (setf *asys* nil)))

;; ---------------------------------
;; top-level functions
;; ---------------------------------

(defun knx-conn-init (host &key (port 3671)
                             (start-receive t)
                             (enable-heartbeat t)
                             (tunnel-request-listeners nil))
  "Initialize and setup the KNX connection and other internal structures.
It will make an UDP connection to KNX/IP gateway and establish a tunnelling connection."
  (log:info "Initializing KNX...")
  (unless (ip-connect host port)
    (error "Could not connect to KNX/IP"))
  (%ensure-asys)
  (unless *async-handler*
    (log:info "Creating async-handler...")
    (make-async-handler *asys*))
  (when tunnel-request-listeners
    (dolist (listener-fun tunnel-request-listeners)
      (add-tunnelling-request-listener listener-fun)))
  (when start-receive
    (log:info "Starting async-receive...")
    (start-async-receive))
  (fawait (establish-tunnel-connection enable-heartbeat) :timeout 10)
  (unless (tunnel-connection-established-p)
    (error "Could not establish tunnel connection!")))

(defun knx-conn-destroy ()
  "Close the KNX connection and destroy the internal structures."
  (log:info "Destroying KNX...")
  ;; close stuff
  (ignore-errors
   (fawait (close-tunnel-connection) :timeout 5))
  (clr-tunnelling-request-listeners)
  (ignore-errors
   (ip-disconnect))
  (ignore-errors
   (%shutdown-asys))
  (when *async-handler*
    (setf *async-handler* nil))
  ;; reset stuff
  (setf knx-client:*receive-knx-data-recur-delay-secs*
        knx-client:*default-receive-knx-data-recur-delay-secs*)
  (setf knx-client::*heartbeat-interval-secs*
        knx-client::+default-heartbeat-interval-secs+))

;; ---------------------------------
;; convenience functions and macro DSL
;; ---------------------------------

(defun write-value (group-address dpt-type value &key (sync nil))
  "Write the given `value` to the `group-address` with the given `dpt-type`.
`sync`: send the request aynchronous. Block until it has been sent."
  (log:info "Writing value: ~a (~a) to ga: ~a" value dpt-type group-address)
  (send-write-request (address:make-group-address group-address)
                      (cond
                        ((eq dpt-type 'dpt:dpt-1.001)
                         (dpt:make-dpt1 dpt-type (if value :on :off))))
                      :sync sync))

(defmacro %make-listener-fun (requested-ga dpt-type)
  `(lambda (req)
     (tasks:with-context (*asys* :read-request)
       (tasks:task-async
        (lambda ()
          (let* ((cemi (tunnelling-request-cemi req))
                 (ga (cemi-destination-addr cemi)))
            (log:debug "Received request for ga: ~a" ga)
            (when (equalp ga ,requested-ga)
              (rem-tunnelling-request-listener listener-fun)
              (handler-case
                  (progn
                    (log:debug "Matches requested ga: ~a" ga)
                    (let* ((cemi-data (cemi-data cemi))
                           (dpt (etypecase cemi-data
                                  (dpt
                                   cemi-data)
                                  ((vector octet)
                                   (parse-to-dpt ,dpt-type
                                                 cemi-data))))
                           (value (dpt-value dpt)))
                      (log:debug "Received requested value: ~a for ga: ~a"
                                 dpt group-address)
                      (fresolve value)))
                (error (e)
                  (log:error "Error in listener-fun: ~a" e)
                  (fresolve e))))))))))

(defun request-value (group-address dpt-type)
  "Request the value of the given `group-address` with the given `dpt-type`.

Returns `sento.future:future` that will be resolved with the value when it is received.
In case of error, the future will be resolved with the error condition or `NIL' if the future times out."
  (log:info "Requesting value for ga: ~a" group-address)
  (let* ((requested-ga (make-group-address group-address))
         (listener-fun))
    (fcompleted
        (with-fut-resolve
          (setf listener-fun
                (%make-listener-fun requested-ga dpt-type))
          (add-tunnelling-request-listener listener-fun)
          (send-read-request (make-group-address group-address)))
        (result)
      (declare (ignore result))
      (log:debug "request-value completed"))))

(defmacro with-knx/ip ((host &key (port 3671)) &body body)
  "Macro that initializes and destroys a KNX/IP connection.
Use the `body' to perform operations on the KNX connection,
i.e. `write-value' or `request-value'."
  `(unwind-protect
        (progn
          (knx-conn-init ,host :port ,port
                               :start-receive t
                               :enable-heartbeat nil)
          ,@body)
     (knx-conn-destroy)))
