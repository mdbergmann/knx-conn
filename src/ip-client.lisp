(defpackage :knx-conn.ip-client
  (:use :cl :knxobj)
  (:documentation "KNX IP client package")
  (:nicknames :ip-client)
  (:export #:ip-connect
           #:ip-disconnect
           #:ip-connected-p
           #:ip-send-knx-data
           #:ip-receive-knx-data
           #:*ctrl-local-host-and-port*
           #:*data-local-host-and-port*))

(in-package :knx-conn.ip-client)

;; -----------------------------
;; low-level communication (UDP)
;; -----------------------------

(defvar *ctrl-conn* nil)
(defvar *data-conn* nil)
(defvar *ctrl-local-host-and-port* nil)
(defvar *data-local-host-and-port* nil)

;; (defgeneric ip-connect (channel address &optional port)
;;   (:documentation "Connect to the KNXnet/IP gateway at the given `address` and `port`."))

(defun ip-connect (address &optional (port 3671))
  (assert (null *ctrl-conn*) nil "Already connected!")
  (let ((conn (usocket:socket-connect
               address port
               :protocol :datagram
               :element-type 'octet)))
    (assert conn nil "Could not ip-connect to ~a on port ~a" address port)
    (log:info "Connected to ~a on port ~a" address port)
    (setf *ctrl-conn* conn)
    (setf *ctrl-local-host-and-port*
          (cons (usocket:get-local-name conn)
                (usocket:get-local-port conn))))
  (let ((conn (usocket:socket-connect
               address port
               :protocol :datagram
               :element-type 'octet)))
    (assert conn nil "Could not ip-connect to ~a on port ~a" address port)
    (log:info "Connected to ~a on port ~a" address port)
    (setf *data-conn* conn)
    (setf *data-local-host-and-port*
          (cons (usocket:get-local-name conn)
                (usocket:get-local-port conn))))
  (log:info "channel (ctrl): local-host, port: ~a"
            *ctrl-local-host-and-port*)
  (log:info "channel (data): local-host, port: ~a"
            *data-local-host-and-port*)
  (and *ctrl-conn* *data-conn*))

  ;; (let ((channel (ecase channel-type
  ;;                  (:ctrl *ctrl-conn*)
  ;;                  (:data *data-conn*))))
  ;;   (assert (null channel) nil "Already connected!")
  ;;   (let ((conn (usocket:socket-connect
  ;;                address port
  ;;                :protocol :datagram
  ;;                :element-type 'octet)))
  ;;     (assert conn nil "Could not ip-connect to ~a on port ~a for channel type: ~a"
  ;;             address port channel-type)
  ;;     (ecase channel-type
  ;;       (:ctrl (setf *ctrl-conn* conn)
  ;;              (setf *ctrl-local-host-and-port*
  ;;                    (cons (usocket:get-local-name conn)
  ;;                          (usocket:get-local-port conn))))
  ;;       (:data (setf *data-conn* conn)
  ;;              (setf *data-local-host-and-port*
  ;;                    (cons (usocket:get-local-name conn)
  ;;                          (usocket:get-local-port conn))))))
  ;;   (log:info "channel (ctrl): local-host, port: ~a"
  ;;             *ctrl-local-host-and-port*)
  ;;   (log:info "channel (data): local-host, port: ~a"
  ;;             *data-local-host-and-port*)
  ;;   (log:debug "Conn (ctrl): ~a" *ctrl-conn*)
  ;;   (log:debug "Conn (data): ~a" *data-conn*)
  ;;   channel))

;; (defmethod ip-connect ((channel (eql :ctrl)) address &optional (port 3671))
;;   "Connect to the KNXnet/IP gateway at the given `address` and `port`."
;;   (%ip-connect :ctrl address port))

;; (defmethod ip-connect ((channel (eql :data)) address &optional (port 3671))
;;   "Connect to the KNXnet/IP gateway at the given `address` and `port`."
;;   (%ip-connect :data address port))

(defun ip-disconnect ()
  "Disconnect from the KNXnet/IP gateway."
  (log:info "Disconnecting from KNXnet/IP gateway")
  (when *ctrl-conn*
    (log:info "Closing ctrl connection")
    (usocket:socket-close *ctrl-conn*))
  (when *data-conn*
    (log:info "Closing data connection")
    (usocket:socket-close *data-conn*))
  (setf *ctrl-conn* nil
        *data-conn* nil)
  (setf *ctrl-local-host-and-port* nil
        *data-local-host-and-port* nil))

(defgeneric ip-connected-p (channel)
  (:documentation "Return whether the client is connected to the KNXnet/IP gateway."))

(defmethod ip-connected-p ((channel (eql :ctrl)))
  "Return whether the client is connected to the KNXnet/IP gateway."
  (not (null *ctrl-conn*)))

(defmethod ip-connected-p ((channel (eql :data)))
  "Return whether the client is connected to the KNXnet/IP gateway."
  (not (null *data-conn*)))

(defgeneric ip-send-knx-data (channel request)
  (:documentation "Send the given `request` to the KNXnet/IP gateway."))

(defun %ip-send-knx-data (channel-type request)
  (let ((conn (ecase channel-type
                (:ctrl *ctrl-conn*)
                (:data *data-conn*))))
    (assert conn nil "Not connected!")
    (log:debug "Sending obj (~a): ~a" (type-of request) channel-type)
    (let ((req-bytes (to-byte-seq request)))
      (check-type req-bytes (simple-array octet (*)))
      (log:debug "Sending bytes (~a): ~a" req-bytes channel-type)
      (usocket:socket-send conn req-bytes (length req-bytes)))
    request))

(defmethod ip-send-knx-data ((channel (eql :ctrl)) request)
  "Send the given `request` to the KNXnet/IP gateway."
  (%ip-send-knx-data :ctrl request))

(defmethod ip-send-knx-data ((channel (eql :data)) request)
  "Send the given `request` to the KNXnet/IP gateway."
  (%ip-send-knx-data :data request))

(defgeneric ip-receive-knx-data (channel)
  (:documentation "Receive a KNXnet/IP request from the KNXnet/IP gateway."))

(defun %ip-receive-knx-data (channel-type)
  (let ((conn (ecase channel-type
                (:ctrl *ctrl-conn*)
                (:data *data-conn*))))
    (assert conn nil "~a not connected!" channel-type)
    (let ((buf (make-array 256 :element-type 'octet)))
      (handler-case 
          (let ((received-obj
                  (parse-root-knx-object
                   (usocket:socket-receive conn buf 256))))
            (log:debug "Received obj type (~a): ~a" (type-of received-obj) channel-type)
            (log:trace "Received bytes (~a): ~a" buf channel-type)
            `(,received-obj nil))
        (error (e)
          (log:warn "Error (~a): ~a" e channel-type)
          (when (loop :for v :across buf
                      :if (not (zerop v))
                        :do (return t)
                      :finally (return nil))
            (log:debug "Received data (~a): ~a" buf channel-type))
          `(nil ,e))
        (condition (c)
          (log:info "Condition (~a): ~a" c channel-type)
          `(nil ,c))))))

(defmethod ip-receive-knx-data ((channel (eql :ctrl)))
  "Receive a KNXnet/IP request from the KNXnet/IP gateway.
Returns a list of the received object and an error condition, if any."
  (%ip-receive-knx-data :ctrl))

(defmethod ip-receive-knx-data ((channel (eql :data)))
  "Receive a KNXnet/IP request from the KNXnet/IP gateway.
Returns a list of the received object and an error condition, if any."
  (%ip-receive-knx-data :data))
