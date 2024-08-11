(defpackage :knx-conn.ip-client
  (:use :cl :knxobj)
  (:documentation "KNX IP client package")
  (:nicknames :ip-client)
  (:export #:ip-connect
           #:ip-disconnect
           #:ip-connected-p
           #:ip-send-knx-data
           #:ip-receive-knx-data
           #:*local-host-and-port*))

(in-package :knx-conn.ip-client)

;; -----------------------------
;; low-level communication (UDP)
;; -----------------------------

(defvar *conn* nil)
(defvar *local-host-and-port* nil)

(defun ip-connect (address &optional (port 3671))
  (assert (null *conn*) nil "Already connected!")
  (let ((conn (usocket:socket-connect
               address port
               :protocol :datagram
               :element-type 'octet)))
    (assert conn nil "Could not ip-connect to ~a on port ~a" address port)
    (log:info "Connected to ~a on port ~a" address port)
    (setf *local-host-and-port*
          (cons (usocket:get-local-name conn)
                (usocket:get-local-port conn)))
    (log:info "channel local-host, port: ~a"
              *local-host-and-port*)
    (setf *conn* conn)))

(defun ip-disconnect ()
  "Disconnect from the KNXnet/IP gateway."
  (log:info "Disconnecting from KNXnet/IP gateway")
  (when *conn*
    (log:info "Closing connection")
    (usocket:socket-close *conn*))
  (setf *conn* nil)
  (setf *local-host-and-port* nil))

(defun ip-connected-p ()
  "Return whether the client is connected to the KNXnet/IP gateway."
  (not (null *conn*)))

(defun ip-send-knx-data (request)
  (let ((conn *conn*))
    (assert conn nil "Not connected!")
    (log:debug "Sending obj: ~a" (type-of request))
    (let ((req-bytes (to-byte-seq request)))
      (check-type req-bytes (simple-array octet (*)))
      (log:trace "Sending bytes: ~a" req-bytes)
      (usocket:socket-send conn req-bytes (length req-bytes)))
    request))

(defun ip-receive-knx-data ()
  (let ((conn *conn*))
    (assert conn nil "Not connected!")
    (let ((buf (make-array 256 :element-type 'octet)))
      (log:debug "Receiving...")
      (handler-case 
          (let ((received-obj
                  (parse-root-knx-object
                   (usocket:socket-receive conn buf 256))))
            (log:debug "Received obj type: ~a" (type-of received-obj))
            (log:trace"Received bytes: ~a" buf)
            `(,received-obj nil))
        (error (e)
          (log:warn "Error: ~a" e)
          (when (loop :for v :across buf
                      :if (not (zerop v))
                        :do (return t)
                      :finally (return nil))
            (log:debug "Received data: ~a" buf))
          `(nil ,e))
        (condition (c)
          (log:info "Condition: ~a" c)
          `(nil ,c))))))
