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
           ;; dpt5
           #:dpt5
           #:dpt5-p
           #:make-dpt5
           ;; dpt9
           #:dpt9
           #:dpt9-p
           #:make-dpt9
           ;; dpt10
           #:dpt10
           #:dpt10-p
           #:make-dpt10
           ;; dpt11
           #:dpt11
           #:dpt11-p
           #:make-dpt11
           ;; value/dpt types
           #:dpt-1.001
           #:dpt-5.001
           #:dpt-5.010
           #:dpt-9.001
           #:dpt-10.001
           #:dpt-11.001
           ;; conditions
           #:dpt-out-of-bounds-error))

(in-package :knx-conn.dpt)

;; value types ----------------------------
;; enforce the value types to be a symbol and only defined here.

(defparameter *dpt-supported-value-types*
  '((:switch . dpt-1.001)
    (:scaling . dpt-5.001)
    (:ucount . dpt-5.010)
    (:temperature . dpt-9.001)
    (:time-of-day . dpt-10.001)
    (:date . dpt-11.001)
    ))

(defun %named-value-sym-for-dpt-sym (sym)
  (car (or
        (find sym
              *dpt-supported-value-types* :test #'eq :key #'cdr)
        (list sym))))

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
  "Assert that the value is within the bounds.
The function `bounds-fun', called with the value should make sure that
the value is within the desired value bounds.
If it is, the function should return `T' and `NIL' if it is not."
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

(defun %assert-byte-vec (value-type byte-vec len)
  (unless (= (length byte-vec) len)
    (error 'knx-unable-to-parse
           :format-control (format nil "Byte vector must be of length ~a" len)
           :format-arguments (list value-type))))

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
  (%assert-byte-vec value-type byte-vec 1)
  (let ((value (if (zerop (aref byte-vec 0))
                   :off
                   :on)))
    (make-dpt1 value-type value)))

(defun make-dpt1 (value-sym value)
  "supported `value-sym': `(or :switch 'dpt-1.001)` as switch with `:on` or `:off` values."
  (ecase (%named-value-sym-for-dpt-sym value-sym)
    (:switch
        (%make-dpt1 :value-type 'dpt-1.001
                    :value value
                    :raw-value (ecase value
                                 (:on 1)
                                 (:off 0))))))

(defun dpt1-toggle (dpt)
  "Toggle the value of the DPT1: `:on` -> `:off` and vise versa."
  (make-dpt1 (dpt-value-type dpt)
             (case (dpt-value dpt)
               (:on :off)
               (:off :on))))

;; ------------------------------
;; DPT5
;; ------------------------------

(defstruct (dpt5 (:include dpt)
                 (:constructor %make-dpt5))
  "Data Point Type 5 for '8-Bit Unsigned Value' (1 Octets)"
  (raw-value (error "Required raw-value!") :type (vector octet 1))
  (value (error "Required value!") :type octet))

(defparameter *scale-factor-5.001* (/ 100 255))

(defun make-dpt5 (value-sym value)
  "5.001 Scaling (%) values: 0-100,
5.010 Value_1_Ucount values: 0-255
`VALUE-SYM' can be:
- `:scaling' or `dpt-5.001'
- `:count' or `dpt-5.010'."
  (declare (octet value))
  (check-type value octet)
  (ecase (%named-value-sym-for-dpt-sym value-sym)
    (:scaling
     (%with-bounds-check
         (lambda () (<= 0 value 100))
       (%make-dpt5 :value-type 'dpt-5.001
                   :raw-value (seq-to-array
                               (vector (truncate
                                        (/ value *scale-factor-5.001*)))
                               :len 1)
                   :value value)))
    (:ucount
     (%make-dpt5 :value-type 'dpt-5.010
                 :raw-value (seq-to-array (vector value) :len 1)
                 :value value))))

(defmethod dpt-byte-len ((dpt dpt5))
  1)

(defmethod dpt-value ((dpt dpt5))
  (dpt5-value dpt))

(defmethod to-byte-seq ((dpt dpt5))
  (dpt5-raw-value dpt))

(defmethod parse-to-dpt ((value-type (eql 'dpt-5.001)) byte-vec)
  (%assert-byte-vec value-type byte-vec 1)
  (log:debug "Byte vector for DPT5.001: ~a" byte-vec)
  (%make-dpt5 :value-type value-type
              :raw-value (seq-to-array byte-vec :len 1)
              :value (truncate
                      (* (elt byte-vec 0)
                         *scale-factor-5.001*))))

(defmethod parse-to-dpt ((value-type (eql 'dpt-5.010)) byte-vec)
  (%assert-byte-vec value-type byte-vec 1)
  (log:debug "Byte vector for DPT5.010: ~a" byte-vec)
  (%make-dpt5 :value-type value-type
              :raw-value (seq-to-array byte-vec :len 1)
              :value (elt byte-vec 0)))

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
  (%assert-byte-vec value-type byte-vec 2)
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
      (log:trace "Exponent: ~a" exponent)
      (log:trace "Mantissa: ~a" mantissa)
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
      (log:trace "Exponents for '~a': ~a" value exponent)
      (log:trace "Scaled value for '~a': ~a" value scaled-value)

      (let ((mantissa (logand (round scaled-value) #x7ff)))
        (log:trace "Mantissa for '~a': ~a" value mantissa)
        (let ((high-byte (->
                           (if value-negative #x80 #x00)
                           (logior (ash exponent 3)
                                   (ash mantissa -8)))))
          (log:trace "High byte for '~a': ~a" value high-byte)

          (let ((low-byte (logand mantissa #xff)))
            (log:trace "Low byte for '~a': ~a" value low-byte)
            (vector high-byte low-byte)))))))

(defun make-dpt9 (value-sym value)
  "9.001 Temperature (°C)
`VALUE-SYM' can be `:temperature' for 9.001."
  (declare (float value))
  (ecase (%named-value-sym-for-dpt-sym value-sym)
    (:temperature
     (%with-bounds-check
         (lambda () (<= -273.0 value 670760.96))
       (%make-dpt9 :value-type 'dpt-9.001
                   :raw-value (seq-to-array
                               (%make-dpt9-double-octet-float-value value)
                               :len 2)
                   :value value)))))

;; ------------------------------
;; DPT10
;; ------------------------------

(defstruct (dpt10 (:include dpt)
                  (:constructor %make-dpt10))
  "10.001 Time Of Day
            +-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+
Field Names | (Day)       (Hour)            |
Encoding    | N   N   N   U   U   U   U   U |
            +-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+
            | 0   0   (Minutes)             |
            |         U   U   U   U   U   U |
            +-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+
            | 0   0   (Seconds)             |
            |         U   U   U   U   U   U |
            +---+---+---+---+---+---+---+---+
Format:     3 octets (N3 U5 r2 U6 r2 U6)
Encoding:   Day = [0 .. 7]
            1 = Monday, 2 = Tuesday, 3 = Wednesday, 4 = Thursday, 5 = Friday, 6 = Saturday, 7 = Sunday, 0 = no day
            Hour    = [0 .. 23]
            Minutes = [0 .. 59]
            Seconds = [0 .. 59]"
  (raw-value (error "Required raw-value!") :type (vector octet 3))
  (value (error "Required value!") :type local-time:timestamp))

(defmethod dpt-byte-len ((dpt dpt10))
  3)

(defmethod dpt-value ((dpt dpt10))
  (dpt10-value dpt))

(defmethod dpt-raw-value ((dpt dpt10))
  (dpt10-raw-value dpt))

(defmethod dpt-supports-optimized-p ((dpt dpt10))
  nil)

(defmethod to-byte-seq ((dpt dpt10))
  (dpt10-raw-value dpt))

(defmethod parse-to-dpt ((value-type (eql 'dpt-10.001)) byte-vec)
  (%assert-byte-vec value-type byte-vec 3)
  (log:debug "Byte vector for DPT10.001: ~a" byte-vec)
  (let* ((oct3 (aref byte-vec 0))
         (day (ash (logand oct3 #x70) -5))
         (hour (logand oct3 #x1f))
         (minute (aref byte-vec 1))
         (second (aref byte-vec 2))
         (ts (local-time:now)))
    (when (> day 0)
      (local-time:adjust-timestamp! ts
        (offset :day-of-week (aref local-time::+day-names-as-keywords+
                                   (if (= day 7) 0 day)))))
    (local-time:adjust-timestamp! ts
      (set :hour hour)
      (set :minute minute)
      (set :sec second))
    (%make-dpt10 :value-type value-type
                 :raw-value (seq-to-array byte-vec :len 3)
                 :value ts)))

(defun %timestamp-to-dpt10 (timestamp)
  (let ((day (local-time:timestamp-day-of-week timestamp))
        (hour (local-time:timestamp-hour timestamp))
        (minute (local-time:timestamp-minute timestamp))
        (second (local-time:timestamp-second timestamp)))
    (setf day (if (= day 0) 7 day))
    (vector (logior (ash day 5) (logand hour #x1f))
            (logand minute #x3f)
            (logand second #x3f))))

(defun make-dpt10 (timestamp)
  (check-type timestamp local-time:timestamp)
  (%make-dpt10 :value-type 'dpt-10.001
               :raw-value (seq-to-array
                           (%timestamp-to-dpt10 timestamp)
                           :len 3)
               :value timestamp))

;; ------------------------------
;; DPT11
;; ------------------------------

(defstruct (dpt11 (:include dpt)
                  (:constructor %make-dpt11))
  "11.001 Date
            +-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+
Field Names | 0   0   0   (Day)             |
Encoding    |             U   U   U   U   U |
            +-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+
            | 0   0   0   0   (Month)       |
            |                 U   U   U   U |
            +-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+
            | 0   (Year)                    |
            |     U   U   U   U   U   U   U |
            +---+---+---+---+---+---+---+---+
Format:     3 octets (r3 U5 r4 U4 r1 U7)
Encoding:   Day   = [1 .. 31]
            Month = [1 .. 12]
            Year  = [0 .. 99]
This format covers the range 1990 to 2089. The following interpretation shall be carried out by devices receiving the Data Point Type 11.001 and carrying out calculations on the basis of the entire 3rd octet:  
- If Octet 3 contains value ≥ 90 : interpret as 20th century
- If Octet 3 contains value < 90: interpret as 21st century"
  (raw-value (error "Required raw-value!") :type (vector octet 3))
  (value (error "Required value!") :type local-time:timestamp))

(defmethod dpt-byte-len ((dpt dpt11))
  3)

(defmethod dpt-value ((dpt dpt11))
  (dpt11-value dpt))

(defmethod dpt-raw-value ((dpt dpt11))
  (dpt11-raw-value dpt))

(defmethod dpt-supports-optimized-p ((dpt dpt11))
  nil)

(defmethod to-byte-seq ((dpt dpt11))
  (dpt11-raw-value dpt))

(defun %timestamp-to-dpt11 (timestamp)
  (let ((day (local-time:timestamp-day timestamp))
        (month (local-time:timestamp-month timestamp))
        (year (local-time:timestamp-year timestamp)))
    (vector day
            month
            (- year 2000))))

(defun make-dpt11 (timestamp)
  (check-type timestamp local-time:timestamp)
  (%make-dpt11 :value-type 'dpt-11.001
               :raw-value (seq-to-array
                           (%timestamp-to-dpt11 timestamp)
                           :len 3)
               :value timestamp))

(defmethod parse-to-dpt ((value-type (eql 'dpt-11.001)) byte-vec)
  (%assert-byte-vec value-type byte-vec 3)
  (log:debug "Byte vector for DPT11.001: ~a" byte-vec)
  (let* ((day (logand (aref byte-vec 0) #x1f))
         (month (logand (aref byte-vec 1) #x0f))
         (year (logand (aref byte-vec 2) #x7f))
         (year-offset (if (>= year 90) 1900 2000))
         (ts (local-time:adjust-timestamp (local-time:today)
               (set :day-of-month day)
               (set :month month)
               (set :year (+ year year-offset)))))
    (%make-dpt11 :value-type value-type
                 :raw-value (seq-to-array byte-vec :len 3)
                 :value ts)))
