(defpackage :knx-conn.dpt
  (:use :cl :knxobj :knxutil)
  (:nicknames :dpt)
  (:import-from #:binding-arrows
                #:->)
  (:export #:dpt
           #:dpt-p
           #:dpt-value-type
           #:dpt-value
           #:dpt-raw-value
           #:value-type-string-to-symbol
           #:dpt-byte-len
           #:dpt-supports-optimized-p
           #:parse-to-dpt
           ;; dpt1
           #:dpt1
           #:dpt1-p
           #:make-dpt1
           #:dpt1-toggle
           ;; dpt9
           #:dpt9
           #:dpt9-p
           #:make-dpt9
           ;; dpt5
           #:dpt5
           #:dpt5-p
           #:make-dpt5
           ;; value/dpt types
           #:dpt-1.001
           #:dpt-9.001
           #:dpt-5.001
           ;; conditions
           #:dpt-out-of-bounds-error))

(in-package :knx-conn.dpt)

;; value types ----------------------------
;; enforce the value types to be a symbol and only defined here.

(defparameter *dpt-supported-value-types*
  '((:switch . dpt-1.001)
    (:temperature . dpt-9.001)
    (:scaling . dpt-5.001)))

(defun dpt-value-type-p (value-type)
  "Check if the `VALUE-TYPE' is supported."
  (find value-type *dpt-supported-value-types* :key #'cdr :test #'eq))

(defun value-type-string-to-symbol (value-type-str)
  "Convert the `VALUE-TYPE-STR', i.e. \"1.001\" to a symbol, i.e. `DPT-1.001'."
  (find-symbol (format nil "DPT-~a" value-type-str) :dpt))

(deftype dpt-value-type ()
  "A type for the DPT value type."
  `(satisfies dpt-value-type-p))

;; conditions ----------------------------

(define-condition dpt-out-of-bounds-error (simple-error) ()
  (:report (lambda (condition stream)
             (format stream "DPT value out of bounds: ~a"
                     (simple-condition-format-control condition)))))

(defmacro %with-bounds-check (bounds-fun &body body)
  "Assert that the value is within the bounds."
  `(progn
     (unless (funcall ,bounds-fun)
       (error 'dpt-out-of-bounds-error :format-control "Value out of bounds"))
     ,@body))

;; ------------------------------

(defgeneric parse-to-dpt (value-type byte-vec)
  (:documentation "Parse `byte-vec' to a DPT of `VALUE-TYPE'"))

(defgeneric dpt-byte-len (dpt)
  (:documentation "Return the length of the DPT"))

(defgeneric dpt-value (dpt)
  (:documentation "Return the specific value of the DPT"))

(defgeneric dpt-raw-value (dpt)
  (:documentation "Return the raw value of the DPT"))

(defgeneric dpt-supports-optimized-p (dpt)
  (:documentation "Check if the DPT supports optimized npdu handling."))

(defstruct (dpt (:include knx-obj)
                (:conc-name dpt-)
                (:constructor nil))
  "A DPT is a data point type.
I.e. the value for switches, dimmers, temperature sensors, etc. are all encoded using DPTs. The DPTs are used to encode and decode the data for transmission over the KNX bus."
  (value-type (error "Required value-type") :type dpt-value-type))

;; ------------------------------
;; DPT1
;; ------------------------------

(defstruct (dpt1 (:include dpt)
                 (:constructor %make-dpt1))
  "
            +-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+
Field Names |                             b |
Encoding    |                             B |
            +---+---+---+---+---+---+---+---+
Format:     1 bit (B<sub>1</sub>)
Range:      b = {0 = off, 1 = on}"
  (raw-value (error "Required value!") :type octet)
  (value (error "Required value!") :type (member :on :off)))

(defmethod dpt-byte-len ((dpt dpt1))
  1)

(defmethod dpt-value ((dpt dpt1))
  (dpt1-value dpt))

(defmethod dpt-raw-value ((dpt dpt1))
  (dpt1-raw-value dpt))

(defmethod dpt-supports-optimized-p ((dpt dpt1))
  t)

(defmethod to-byte-seq ((dpt dpt1))
  (vector (dpt1-raw-value dpt)))

(defmethod parse-to-dpt ((value-type (eql 'dpt-1.001)) byte-vec)
  (unless (= (length byte-vec) 1)
    (error 'knx-unable-to-parse
           :format-control "Byte vector must be of length 1"
           :format-arguments (list value-type)))
  (let ((value (if (zerop (aref byte-vec 0))
                   :off
                   :on)))
    (make-dpt1 value-type value)))

(defun make-dpt1 (value-sym value)
  "supported `value-sym': `(or :switch 'dpt-1.001)` as switch with `:on` or `:off` values."
  (cond
    ((member value-sym '(:switch dpt-1.001))
     (%make-dpt1 :value-type 'dpt-1.001
                 :value value
                 :raw-value (ecase value
                              (:on 1)
                              (:off 0))))
    (t (error 'type-error :datum
              (format nil "Unsupported value type: ~a" value-sym)))))

(defun dpt1-toggle (dpt)
  "Toggle the value of the DPT1: `:on` -> `:off` and vise versa."
  (make-dpt1 (dpt-value-type dpt)
             (case (dpt-value dpt)
               (:on :off)
               (:off :on))))

;; ------------------------------
;; DPT9
;; ------------------------------

(defstruct (dpt9 (:include dpt)
                 (:constructor %make-dpt9))
  "Data Point Type 9 for '2-Octet Float Value' (2 Octets)

            +-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+
Field Names | (Float Value)                                                 |
Encoding    | M   E   E   E   E   M   M   M   M   M   M   M   M   M   M   M |
            +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
Format:     2 octets (F_16)
Encoding:   Float Value = (0.01 * M)*2(E)
            E = [0 .. 15]
            M = [-2048 .. 2047], two's complement notation"
  (raw-value (error "Required raw-value!") :type (vector octet 2))
  (value (error "Required value!") :type single-float))

(defmethod dpt-byte-len ((dpt dpt9))
  2)

(defmethod dpt-value ((dpt dpt9))
  (dpt9-value dpt))

(defmethod dpt-raw-value ((dpt dpt9))
  (dpt9-raw-value dpt))

(defmethod dpt-supports-optimized-p ((dpt dpt9))
  nil)

(defmethod to-byte-seq ((dpt dpt9))
  (dpt9-raw-value dpt))

(defmethod parse-to-dpt ((value-type (eql 'dpt-9.001)) byte-vec)
  (unless (= (length byte-vec) 2)
    (error 'knx-unable-to-parse
           :format-control "Byte vector must be of length 2"
           :format-arguments (list value-type)))
  (log:debug "Byte vector for DPT9.001: ~a" byte-vec)
  (labels ((two-completement-or-value (value)
             (if (zerop value)
                 value
                 (1+ (lognot value))))
           (extract-and-normalize-high-byte (high-byte)
             (-> (ash (logand high-byte #x80) 24)
               (two-completement-or-value)
               (logior (ash (logand high-byte #x07) 28))
               (ash -20))))
    (let ((exponent (ash (logand (aref byte-vec 0) #x78) -3))
          (mantissa (let ((high-byte (extract-and-normalize-high-byte
                                      (aref byte-vec 0)))
                          (low-byte (logand (aref byte-vec 1) #xff)))
                      (logior high-byte low-byte))))
      (log:debug "Exponent: ~a" exponent)
      (log:debug "Mantissa: ~a" mantissa)
      (let ((value (* (ash 1 exponent) mantissa 0.01)))
        (%make-dpt9 :value-type value-type
                    :raw-value (seq-to-array byte-vec :len 2)
                    :value value)))))
  
(defun %make-dpt9-double-octet-float-value (value)
  "9.001 Temperature (°C)
Range:      [-273 .. 670760.96]
Unit:       °C
Resolution: 0.01 °C"
  (declare (float value))
  (log:debug "Value for DPT9.001: ~a" value)
  (let* ((scaled-value (* 100 value))
         (value-negative (minusp scaled-value))
         (exponent 0))
    (flet ((loop-scaled (pred-p)
             (loop :for scaled := scaled-value
                     :then (/ scaled 2)
                   :while (funcall pred-p scaled)
                   :do (incf exponent)
                   :finally (return scaled))))
      (setf scaled-value
            (if value-negative
                (loop-scaled (lambda (val) (< val -2048.0)))
                (loop-scaled (lambda (val) (> val 2047.0)))))
      (log:debug "Exponents for '~a': ~a" value exponent)
      (log:debug "Scaled value for '~a': ~a" value scaled-value)

      (let ((mantissa (logand (round scaled-value) #x7ff)))
        (log:debug "Mantissa for '~a': ~a" value mantissa)
        (let ((high-byte (->
                           (if value-negative #x80 #x00)
                           (logior (ash exponent 3)
                                   (ash mantissa -8)))))
          (log:debug "High byte for '~a': ~a" value high-byte)

          (let ((low-byte (logand mantissa #xff)))
            (log:debug "Low byte for '~a': ~a" value low-byte)
            (vector high-byte low-byte)))))))

(defun make-dpt9 (value-sym value)
  "9.001 Temperature (°C)
`VALUE-SYM' can be `:temperature' for 9.001."
  (declare (float value))
  (ecase value-sym
    (:temperature
     (%with-bounds-check
         (lambda () (<= -273.0 value 670760.96))
       (%make-dpt9 :value-type 'dpt-9.001
                   :raw-value (seq-to-array
                               (%make-dpt9-double-octet-float-value value)
                               :len 2)
                   :value value)))))

;; ------------------------------
;; DPT5
;; ------------------------------

(defstruct (dpt5 (:include dpt)
                 (:constructor %make-dpt5))
  "Data Point Type 5 for '8-Bit Unsigned Value' (1 Octets)

"
  (raw-value (error "Required raw-value!") :type (vector octet 1))
  (value (error "Required value!") :type octet))

(defun make-dpt5 (value-sym value)
  "5.001 Scaling (%) 0-100
`VALUE-SYM' can be `:scaling' or `dpt-5.001'."
  (declare (octet value))
  (ecase value-sym
    (:scaling
     (%make-dpt5 :value-type 'dpt-5.001
                 :raw-value (seq-to-array (vector value) :len 1)
                 :value value))))

(defmethod dpt-byte-len ((dpt dpt5))
  1)

(defmethod dpt-value ((dpt dpt5))
  (dpt5-value dpt))

(defmethod to-byte-seq ((dpt dpt5))
  (dpt5-raw-value dpt))
