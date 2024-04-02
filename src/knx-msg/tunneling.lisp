(defpackage :knx-conn.tunnelling
  (:use :cl :knxobj :cemi)
  (:nicknames :tunnelling)
  (:export #:knx-tunnelling-request
           #:make-tunnelling-request
           #:tunnelling-request-conn-header
           #:tunnelling-request-cemi
           #:knx-tunnelling-ack
           #:make-tunnelling-ack
           #:conn-header-channel-id
           #:conn-header-seq-counter
           #:tunnelling-cemi-message-code
           #:tunnelling-channel-id
           #:tunnelling-seq-counter
           ))

(in-package :knx-conn.tunnelling)

(defconstant +knx-tunnelling-request+ #x0420)
(defconstant +knx-tunnelling-ack+ #x0421)

(defconstant +conn-header-structure-len+ #x04)

(defstruct (connection-header (:include knx-obj)
                              (:conc-name conn-header-)
                              (:constructor %make-connection-header))
  "Connection header
+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+
| Structure Length            | Communication Channel ID        |
| (1 octet)                   | (1 octet)                       |
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
| Sequence Counter            | reserved                        |
| (1 octet)                   | (1 octet)                       |
+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+"
  (len +conn-header-structure-len+ :type octet)
  (channel-id (error "Required channel.id!") :type octet)
  (seq-counter (error "Required sequence-counter!") :type octet)
  (reserved 0 :type octet))

(defun %parse-conn-header (data)
  (%make-connection-header
   :channel-id (aref data 1)
   :seq-counter (aref data 2)
   :reserved (aref data 3)))

(defmethod to-byte-seq ((obj connection-header))
  (vector (conn-header-len obj)
          (conn-header-channel-id obj)
          (conn-header-seq-counter obj)
          (conn-header-reserved obj)))

(defstruct (knx-tunnelling-base (:include knx-package)
                                (:conc-name tunnelling-))
  "Base class for KNXnet/IP Tunnelling Request and Acknowledgement
Connection header
+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+
| Structure Length            | Communication Channel ID        |
| (1 octet)                   | (1 octet)                       |
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
| Sequence Counter            | Status                          |
| (1 octet)                   | (1 octet)                       |
+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+"
  (conn-header (error "Required conn-header!") :type connection-header))

(defun tunnelling-channel-id (tunnelling-msg)
  (conn-header-channel-id (tunnelling-conn-header tunnelling-msg)))

(defun tunnelling-seq-counter (tunnelling-msg)
  (conn-header-seq-counter (tunnelling-conn-header tunnelling-msg)))

(defstruct (knx-tunnelling-request (:include knx-tunnelling-base)
                                   (:conc-name tunnelling-request-)
                                   (:constructor %make-tunnelling-request))
  "KNXnet/IP header

KNXnet/IP body
Connection header
+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+
| Structure Length            | Communication Channel ID        |
| (1 octet)                   | (1 octet)                       |
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
| Sequence Counter            | reserved                        |
| (1 octet)                   | (1 octet)                       |
+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+
cEMI frame
+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+
| Message Code                | Additional Info Length          |
| (1 octet = 08h)             | (1 octet)                       |
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
| Additional Information                                        |
| (optional, variable length)                                   |
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
| Service Information                                           |
| (variable length)                                             |
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+"
  (cemi (error "Required cemi!") :type cemi))

(defmethod parse-to-obj ((obj-type (eql +knx-tunnelling-request+)) header body)
  (let ((conn-header (%parse-conn-header
                      (subseq body 0 +conn-header-structure-len+))))
    (%make-tunnelling-request
     :header header
     :conn-header conn-header
     :cemi (parse-cemi (subseq body +conn-header-structure-len+)))))

(defmethod to-byte-seq ((obj knx-tunnelling-request))
  (concatenate '(vector octet)
               (call-next-method obj)
               (to-byte-seq (tunnelling-conn-header obj))
               (to-byte-seq (tunnelling-request-cemi obj))))

(defun make-tunnelling-request (&key channel-id seq-counter cemi)
  (%make-tunnelling-request
   :header (make-header +knx-tunnelling-request+
                        (+ +conn-header-structure-len+
                           (cemi-len cemi)))
   :conn-header (%make-connection-header
                 :channel-id channel-id
                 :seq-counter seq-counter)
   :cemi cemi))

(defun tunnelling-cemi-message-code (tunnel-req)
  (check-type tunnel-req knx-tunnelling-request)
  (cemi-message-code (tunnelling-request-cemi tunnel-req)))

;; -------------------------------
;; Tunnelling ack
;; -------------------------------

(defstruct (knx-tunnelling-ack (:include knx-tunnelling-base)
                               (:conc-name tunnelling-ack-)
                               (:constructor %make-tunnelling-ack))
  "KNXnet/IP header

KNXnet/IP body contains only conn-header, see above.")

(defmethod parse-to-obj ((obj-type (eql +knx-tunnelling-ack+)) header body)
  (let ((conn-header (%parse-conn-header
                      (subseq body 0 +conn-header-structure-len+))))
    (%make-tunnelling-ack
     :header header
     :conn-header conn-header)))

(defmethod to-byte-seq ((obj knx-tunnelling-ack))
  (concatenate '(vector octet)
               (call-next-method obj)
               (to-byte-seq (tunnelling-conn-header obj))))

(defun make-tunnelling-ack (tunnelling-request)
  (let ((request-conn-header
          (copy-structure
           (tunnelling-request-conn-header tunnelling-request))))
    (%make-tunnelling-ack
     :header (make-header +knx-tunnelling-ack+
                          +conn-header-structure-len+)
     :conn-header request-conn-header)))
