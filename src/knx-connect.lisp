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
                                           (:shared (:workers 2 :strategy :round-robin)
                                            :receiver (:workers 1)
                                            :waiter (:workers 1)
                                            :notifier (:worker 1) ;; for listeners
                                            :read-request (:workers 1)))))))

(defun %shutdown-asys ()
  (log:info "Shutting down actor system...")
  (when *asys*
    (ac:shutdown *asys* :wait nil)
    (setf *asys* nil)))

;; ---------------------------------
;; top-level functions
;; ---------------------------------

(defun knx-conn-init (host &key (port 3671)
                             (enable-heartbeat t)
                             (tunnel-request-listeners nil)
                             (group-address-dpt-mapping nil))
  "Initialize and setup the KNX connection and other internal structures.
It will make an UDP connection to KNX/IP gateway and establish a tunnelling connection.

- `host` is the IP address or hostname of the KNXnet/IP gateway.
- `port` is the port number of the KNXnet/IP gateway (default is 3671).
- `enable-heartbeat` is a boolean flag to enable the heartbeat mechanism (default is `T`).
This can be `NIL` only for short time window. Usually the KNXnet/IP gateway wants to have a heartbeat at most every 2 minutes. or it will drop the connection.
- `tunnel-request-listeners` is a list of functions that will be called when a tunnelling request (L_Data.ind) is received. The function takes a single argument, the request object (`tunnelling:knx-tunnelling-request`). It is the responsibility of the listener function to filter the received requests.
- `group-address-dpt-mapping` is a list of group-address to DPT type mappings. The DPT type is used to parse the received data for the group-address. See `knx-client:*group-address-dpt-mapping*` for more information on the data structure. It is possible to also set, or modify the mapping during runtime via the variable directly. Note that this mapping is used for parsing the right DPT type for received L_Data.ind tunnelling requests so that a properly dpt-parsed request can be passed to the tunnel request listeners. If there is no mapping, or tunnelling requests are received that are not in the mapping, the requests will be passed to the listeners with raw data cemi data."
  (log:info "Initializing KNX...")
  (unless (ip-connect host port)
    (error "Could not connect to KNX/IP"))
  (%ensure-asys)
  (reset-client-vars)
  (unless *async-handler*
    (log:info "Creating async-handler...")
    (make-async-handler *asys*))
  (when tunnel-request-listeners
    (dolist (listener-fun tunnel-request-listeners)
      (add-tunnelling-request-listener listener-fun)))
  (setf *group-address-dpt-mapping* group-address-dpt-mapping)
  (log:info "Starting async-receive...")
  (start-async-receive)
  (log:info "Establishing tunnel connection...")
  (fawait (establish-tunnel-connection enable-heartbeat) :timeout 5)
  (unless (tunnel-connection-established-p)
    (error "Could not establish tunnel connection!")))

(defun knx-conn-destroy ()
  "Close the KNX connection and destroy the runtime structures."
  (log:info "Destroying KNX...")
  (when *async-handler*
    (clr-tunnelling-request-listeners))
  (ignore-errors
   (fawait (close-tunnel-connection) :timeout 5))
  (ignore-errors
    (ip-disconnect))
  (ignore-errors
    (%shutdown-asys))
  (when *async-handler*
    (setf *async-handler* nil))
  (reset-client-vars))

;; ---------------------------------
;; convenience functions and macro DSL
;; ---------------------------------

(defun write-value (group-address dpt-type value)
  "Write the given `value` to the `group-address` with the given `dpt-type`.
Returns `future:future` which resolves to `T' is all went well and error condition on error."
  (assert (dpt:dpt-value-type-p dpt-type) nil "Unsupported dpt type!")
  (log:info "Writing value: ~a (~a) to ga: ~a" value dpt-type group-address)
  (fmap
      (send-write-request
       (address:make-group-address group-address)
       (dpt:make-dpt dpt-type value))
      (result)
    (destructuring-bind (resp err) result
        (if resp t err))))

(defmacro %make-listener-fun (requested-ga dpt-type)
  `(lambda (req)
     (tasks:with-context (*asys* :read-request)
       (tasks:task-async
        (lambda ()
          (let* ((cemi (tunnelling-request-cemi req))
                 (ga (cemi-destination-addr cemi)))
            (log:debug "Received request for ga: ~a" ga)
            (when (and (equalp ga ,requested-ga)
                       (eql (tunnelling-cemi-message-code req)
                            +cemi-mc-l_data.ind+)
                       (apci-gv-response-p (cemi-apci cemi)))
              (log:debug "Matches requested ga: ~a" ga)
              (rem-tunnelling-request-listener listener-fun)
              (handler-case
                  (let* ((cemi-data (cemi-data cemi))
                         (dpt (etypecase cemi-data
                                (dpt
                                 cemi-data)
                                ((vector octet)
                                 (parse-to-dpt ,dpt-type
                                               cemi-data))))
                         (value (dpt-value dpt)))
                    (log:info "Received requested value: ~a for ga: ~a"
                               dpt group-address)
                    (fresolve value))
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
          ;; listener is cleaned up in macro
          (send-read-request (make-group-address group-address)))
        (result)
      (declare (ignore result))
      (log:debug "request-value completed"))))

(defmacro with-knx/ip ((host &key (port 3671)) &body body)
  "Macro that wraps `body` between `knx-conn-init` and `knx-conn-destroy`.
It allows `body` to perform operations on the KNX connection, i.e. `write-value' or `request-value'."
  `(unwind-protect
        (progn
          (knx-conn-init ,host :port ,port
                               :enable-heartbeat nil)
          ,@body)
     (knx-conn-destroy)))
