(defpackage :knx-conn.ip-client
  (:use :cl :knxobj)
  (:documentation "KNX IP client package")
  (:nicknames :ip-client)
  (:export #:ip-connect
           #:ip-disconnect
           #:ip-connected-p
           #:ip-send-knx-data
           #:ip-receive-knx-data))

(in-package :knx-conn.ip-client)

;; -----------------------------
;; low-level communication (UDP)
;; -----------------------------

(defvar *conn* nil)

(defun ip-connect (address &optional (port 3671))
  "Connect to the KNXnet/IP gateway at the given `address` and `port`."
  (assert (null *conn*) nil "Already connected!")
  (let ((conn (usocket:socket-connect
               address port
               :protocol :datagram
               :element-type 'octet)))
    (assert conn nil "Could not %ip-connect to ~a on port ~a" address port)
    (log:info "Connected to ~a on port ~a" address port)
    (setf *conn* conn)))

(defun ip-disconnect ()
  "Disconnect from the KNXnet/IP gateway."
  (assert *conn* nil "Not connected!")
  (log:info "Disconnecting from KNXnet/IP gateway")
  (usocket:socket-close *conn*)
  (setf *conn* nil))

(defun ip-connected-p ()
  "Return whether the client is connected to the KNXnet/IP gateway."
  (not (null *conn*)))

(defun ip-send-knx-data (request)
  "Send the given `request` to the KNXnet/IP gateway."
  (assert *conn* nil "Not connected!")
  (log:debug "Sending obj: ~a" request)
  (let ((req-bytes (to-byte-seq request)))
    (check-type req-bytes (simple-array (unsigned-byte 8) (*)))
    (log:debug "Sending bytes: ~a" req-bytes)
    (usocket:socket-send *conn* req-bytes (length req-bytes)))
  request)

(defun ip-receive-knx-data ()
  "Receive a KNXnet/IP request from the KNXnet/IP gateway.
Returns a list of the received object and an error condition, if any."
  (assert *conn* nil "Not connected!")
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
