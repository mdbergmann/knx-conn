(defpackage :knx-conn.dpt
  (:use :cl :knxobj)
  (:nicknames :dpt)
  (:export #:dpt
           #:dpt-value-type
           #:dpt1
           #:dpt-len
           #:dpt-value
           #:dpt-raw-value
           #:make-dpt1
           #:dpt9
           #:make-dpt9))

(in-package :knx-conn.dpt)

(defgeneric dpt-len (dpt)
  (:documentation "Return the length of the DPT"))

(defgeneric dpt-raw-value (dpt)
  (:documentation "Return the raw value of the DPT"))

(defgeneric dpt-value (dpt)
  (:documentation "Return the specific value of the DPT"))

(defstruct (dpt (:include knx-obj)
                (:conc-name dpt-)
                (:constructor nil))
  "A DPT is a data point type.
I.e. the value for switches, dimmers, temperature sensors, etc. are all encoded using DPTs. The DPTs are used to encode and decode the data for transmission over the KNX bus."
  (value-type (error "Required value-type") :type string)
  )

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
  (raw-value (error "Required value!") :type octet))

(defmethod dpt-len ((dpt dpt1))
  1)

(defmethod dpt-raw-value ((dpt dpt1))
  (dpt1-raw-value dpt))

(defmethod dpt-value ((dpt dpt1))
  (ecase (dpt1-raw-value dpt)
    (0 :off)
    (1 :on)))

(defmethod to-byte-seq ((dpt dpt1))
  (vector (dpt1-raw-value dpt)))

(defun make-dpt1 (value-sym value)
  (ecase value-sym
    (:switch
        (%make-dpt1 :value-type "1.001"
                    :raw-value (ecase value
                                 (:on 1)
                                 (:off 0))))))

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
Format:     2 octets (F<sub>16</sub>)
Encoding:   Float Value = (0.01 * M)*2(E)
            E = [0 .. 15]
            M = [-2048 .. 2047], two's complement notation"
  (raw-value (error "Required value!") :type single-float))

(defun %make-dpt9-temperature-raw-value (value)
  "9.001 Temperature (°C)
Range:      [-273 .. 670760.96]
Unit:       °C
Resolution: 0.01 °C"
  (declare (float value))
  value)

(defun make-dpt9 (value-sym value)
  (declare (float value))
  (ecase value-sym
    (:temperature
        (%make-dpt9 :value-type "9.001"
                    :raw-value (%make-dpt9-temperature-raw-value value)))))

