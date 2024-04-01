(defpackage :knx-conn.connect
  (:use :cl :hpai :cri :crd :knxobj)
  (:nicknames :connect)
  (:export #:knx-connect-request
           #:make-connect-request
           #:knx-connect-response
           #:connect-response-channel-id
           #:connect-response-status
           #:connect-response-crd
           #:+connect-status-no-error+
           #:+connect-status-err-conn-type+
           ;; disconnect
           #:make-disconnect-request
           #:knx-disconnect-request
           #:knx-disconnect-response
           #:disconnect-response-status
           #:disconnect-response-channel-id
           ;; connection-state
           #:make-connstate-request
           #:knx-connstate-request
           #:knx-connstate-response
           ))


(in-package :knx-conn.connect)

(defconstant +connect-status-no-error+ #x00)
(defconstant +connect-status-err-conn-id+ #x21)
(defconstant +connect-status-err-conn-type+ #x22)
(defconstant +connect-status-err-conn-option+ #x23)
(defconstant +connect-status-err-no-more-conns+ #x24)

;; -----------------------------
;; knx connect request
;; -----------------------------

(defconstant +knx-connect-request+ #x0205)

(defstruct (knx-connect-request (:include knx-package)
                                (:constructor %make-connect-request)
                                (:conc-name connect-request-))
  "KNXnet/IP header (see above)

KNXnet/IP body
+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+
| HPAI                                                          |
| Control endpoint                                              |
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
| HPAI                                                          |
| Data endpoint                                                 |
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
| CRI                                                           |
| Connection request information                                |
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+"
  (hpai-ctrl-endpoint (error "Required hpai ctrl endpoint!") :type hpai)
  (hpai-data-endpoint (error "Required hpai data endpoint!") :type hpai)
  (cri (error "Required cri!") :type cri))

(defun make-connect-request (addr-pair-ctrl addr-pair-data)
  (let ((ctrl-hpai (make-hpai (car addr-pair-ctrl) (cdr addr-pair-ctrl)))
        (data-hpai (make-hpai (car addr-pair-data) (cdr addr-pair-data)))
        (cri (make-tunneling-cri)))
    (%make-connect-request
     :header (make-header +knx-connect-request+
                          (+ (hpai-len ctrl-hpai)
                             (hpai-len data-hpai)
                             (cri-len cri)))
     :hpai-ctrl-endpoint ctrl-hpai
     :hpai-data-endpoint data-hpai
     :cri cri)))

(defmethod to-byte-seq ((obj knx-connect-request))
  (concatenate '(vector octet)
               (call-next-method obj)
               (to-byte-seq (connect-request-hpai-ctrl-endpoint obj))
               (to-byte-seq (connect-request-hpai-data-endpoint obj))
               (to-byte-seq (connect-request-cri obj))))

;; -----------------------------
;; knx connect response
;; -----------------------------

(defconstant +knx-connect-response+ #x0206)

(defstruct (knx-connect-response (:include knx-package)
                                 (:constructor %make-connect-response)
                                 (:conc-name connect-response-))
  "Connect response

KNXnet/IP header (see knx-header)

KNXnet/IP body
+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+
| Communication Channel ID    | Status                          |
|                             |                                 |
+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+
| HPAI                                                          |
| Data endpoint                                                 |
+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+
| CRD                                                           |
| Connection Response Data Block                                |
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+"
  (channel-id (error "channel-id required!") :type octet)
  (status (error "status required!") :type octet)
  (hpai (error "hpai required!") :type hpai)
  (crd (error "crd required!") :type crd))

(defmethod parse-to-obj ((obj-type (eql +knx-connect-response+)) header body)
  (let ((channel-id (aref body 0))
        (status (aref body 1)))
    (%make-connect-response
     :header header
     :channel-id channel-id
     :status status
     :hpai (parse-hpai (subseq body 2 10))
     :crd (parse-crd (subseq body 10)))))

(defmethod to-byte-seq ((obj knx-connect-response))
  (concatenate '(vector octet)
               (call-next-method obj)
               (vector (connect-response-channel-id obj)
                       (connect-response-status obj))
               (to-byte-seq (connect-response-hpai obj))
               (to-byte-seq (connect-response-crd obj))))

;; -----------------------------
;; knx disconnect request
;; -----------------------------

(defconstant +knx-disconnect-request+ #x0209)

(defstruct (knx-disconnect-request (:include knx-package)
                                   (:constructor %make-disconnect-request)
                                   (:conc-name disconnect-request-))
  "KNXnet/IP header (see above)

KNXnet/IP body
+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+
| Communication Channel ID    | reserved                        |
|                             |                                 |
+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+
| HPAI                                                          |
| Control endpoint                                              |
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+"
  (channel-id (error "channel-id required!") :type octet)
  (hpai *hpai-unbound-addr* :type hpai))

(defun make-disconnect-request (channel-id local-addr-pair)
  (let ((hpai (make-hpai (car local-addr-pair) (cdr local-addr-pair))))
    (%make-disconnect-request
     :header (make-header +knx-disconnect-request+
                          (+ (hpai-len hpai) 2))
     :channel-id channel-id
     :hpai hpai)))

(defmethod to-byte-seq ((obj knx-disconnect-request))
  (concatenate '(vector octet)
               (call-next-method obj)
               (vector (disconnect-request-channel-id obj) 0)
               (to-byte-seq (disconnect-request-hpai obj))))

(defmethod parse-to-obj ((obj-type (eql +knx-disconnect-request+)) header body)
  (let ((channel-id (aref body 0)))
    (%make-disconnect-request
     :header header
     :channel-id channel-id
     :hpai (parse-hpai (subseq body 2)))))

;; -----------------------------
;; knx disconnect response
;; -----------------------------

(defconstant +knx-disconnect-response+ #x020a)

(defstruct (knx-disconnect-response (:include knx-package)
                                    (:constructor %%make-disconnect-response)
                                    (:conc-name disconnect-response-))
  "KNXnet/IP header (see above)

KNXnet/IP body
+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+
| Communication Channel ID    | Status                          |
|                             |                                 |
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+"
  (channel-id (error "channel-id required!") :type octet)
  (status (error "status required!") :type octet))

(defun %make-disconnect-response (channel-id status)
  (%%make-disconnect-response
   :header (make-header +knx-disconnect-response+
                        (+ 6 2))
   :channel-id channel-id
   :status status))

(defmethod parse-to-obj ((obj-type (eql +knx-disconnect-response+)) header body)
  (let ((channel-id (aref body 0))
        (status (aref body 1)))
    (%%make-disconnect-response
     :header header
     :channel-id channel-id
     :status status)))

;; -----------------------------
;; knx connection-state request
;; -----------------------------

(defconstant +knx-connstate-request+ #x0207)

(defstruct (knx-connstate-request (:include knx-package)
                                  (:constructor %make-connstate-request)
                                  (:conc-name connstate-request-))
  "KNXnet/IP header (see above)

KNXnet/IP body
+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+
| Communication Channel ID    | reserved                        |
|                             |                                 |
+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+
| HPAI                                                          |
| Control endpoint                                              |
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+"
  (channel-id (error "channel-id required!") :type octet)
  (hpai *hpai-unbound-addr* :type hpai))

(defun make-connstate-request (channel-id local-addr-pair)
  (let ((hpai (make-hpai (car local-addr-pair) (cdr local-addr-pair))))
    (%make-connstate-request
     :header (make-header +knx-connstate-request+
                          (+ (hpai-len hpai) 2))
     :channel-id channel-id
     :hpai hpai)))

(defmethod to-byte-seq ((obj knx-connstate-request))
  (concatenate '(vector octet)
               (call-next-method obj)
               (vector (connstate-request-channel-id obj) 0)
               (to-byte-seq (connstate-request-hpai obj))))

;; -----------------------------
;; knx connection-state response
;; -----------------------------

(defconstant +knx-connstate-response+ #x0208)

(defstruct (knx-connstate-response (:include knx-package)
                                   (:constructor %%make-connstate-response)
                                   (:conc-name connstate-response-))
  "KNXnet/IP header (see above)

KNXnet/IP body
+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+
| Communication Channel ID    | Status                          |
|                             |                                 |
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+"
  (channel-id (error "channel-id required!") :type octet)
  (status (error "status required!") :type octet))

(defun %make-connstate-response (channel-id status)
  (%%make-connstate-response
   :header (make-header +knx-connstate-response+
                        (+ 6 2))
   :channel-id channel-id
   :status status))

(defmethod parse-to-obj ((obj-type (eql +knx-connstate-response+)) header body)
  (let ((channel-id (aref body 0))
        (status (aref body 1)))
    (%%make-connstate-response
     :header header
     :channel-id channel-id
     :status status)))
