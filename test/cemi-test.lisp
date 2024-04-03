(defpackage :knx-conn.cemi-test
  (:use :cl :fiveam :knx-conn.cemi :address :dpt)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :knx-conn.cemi-test)

(def-suite cemi-tests
  :description "CEMI tests"
  :in knx-conn.tests:test-suite)

(in-suite cemi-tests)

;; CEMI{messageCode=L_DATA_IND, additionalInfo=AdditionalInfo{bytes=}, controlByte1=ControlByte1{standardFrame=true, repeatEnabled=false, broadcastType=NORMAL, priority=LOW, requestAcknowledge=false, errorConfirmation=false}, controlByte2=ControlByte2{addressType=GROUP, hopCount=5, frameFormat=0}, sourceAddress=IndividualAddress{address=1.3.14}, destinationAddress=GroupAddress{address=1034, address(2-level)=0/1034, address(3-level)=0/4/10}, npduLength=3, tpci=UNNUMBERED_PACKAGE, packetNumber=0, apci=GROUP_VALUE_WRITE, data=0x0C 68}

(defparameter *l-data.ind-gv-write*
  #(41 0 188 208 19 14 4 10 3 0 128 12 104))

(test parse-l-data.ind-gv-write
  (let ((cut (parse-cemi *l-data.ind-gv-write*)))
    (print cut)
    (is (typep cut 'cemi-l-data))
    (is (= (cemi-message-code cut) +cemi-mc-l_data.ind+))
    
    (is (equal (cemi-ctrl1 cut) #*10111100))
    (is (equalp `(:standard-frame t
                  :repeat-enabled nil
                  :broadcast-type ,+broadcast-type-normal+
                  :priority ,+priority-low+
                  :ack-requested nil
                  :error-confirmation nil)
                (ctrl1-rep cut)))

    (is (equal (cemi-ctrl2 cut) #*11010000))
    (is (equalp `(:address-type knx-group-address
                  :hop-count 5
                  :frame-format 0)
                (ctrl2-rep cut)))

    (is (typep (cemi-source-addr cut) 'knx-individual-address))
    (is (typep (cemi-destination-addr cut) 'knx-group-address))
    (is (string= (address-string-rep
                  (cemi-source-addr cut)) "1.3.14"))
    (is (string= (address-string-rep
                  (cemi-destination-addr cut)) "0/4/10")) ; Temp_EG_Az

    (is (equalp (cemi-tpci cut) +tcpi-udt+))
    (is (= (cemi-packet-num cut) 0))
    (is (apci-gv-write-p (cemi-apci cut)))
    (is (equalp #(12 104) (cemi-data cut)))
    ))

(defparameter *l-data.ind-gv-write-2*
  #(41 0 188 208 19 14 0 4 1 0 129))

(test parse-l-data.ind-gv-write-2
  (let ((cut (parse-cemi *l-data.ind-gv-write-2*)))
    (print cut)
    (is (typep cut 'cemi-l-data))
    (is (= (cemi-message-code cut) +cemi-mc-l_data.ind+))
    
    (is (equal (cemi-ctrl1 cut) #*10111100))
    (is (equalp `(:standard-frame t
                  :repeat-enabled nil
                  :broadcast-type ,+broadcast-type-normal+
                  :priority ,+priority-low+
                  :ack-requested nil
                  :error-confirmation nil)
                (ctrl1-rep cut)))
    (is (equal (cemi-ctrl2 cut) #*11010000))
    (is (equalp `(:address-type knx-group-address
                  :hop-count 5
                  :frame-format 0)
                (ctrl2-rep cut)))
    (is (typep (cemi-source-addr cut) 'knx-individual-address))
    (is (typep (cemi-destination-addr cut) 'knx-group-address))
    (is (string= (address-string-rep
                  (cemi-source-addr cut)) "1.3.14"))
    (is (string= (address-string-rep
                  (cemi-destination-addr cut)) "0/0/4")) ; Light_EG_Az
    (is (equalp (cemi-tpci cut) +tcpi-udt+))
    (is (= (cemi-packet-num cut) 0))
    (is (apci-gv-write-p (cemi-apci cut)))
    (is (equalp #(1) (cemi-data cut)))
    ))

(defparameter *l-data.con-gv-read*
  #(46 0 188 224 238 255 0 4 1 0 0 0 0)
  "Does not contain any data")

(test parse-l-data.con-gv-read
  (let ((cut (parse-cemi *l-data.con-gv-read*)))
    (print cut)
    (is (typep cut 'cemi-l-data))
    (is (= (cemi-message-code cut) +cemi-mc-l_data.con+))
    
    (is (equal (cemi-ctrl1 cut) #*10111100))
    (is (equalp `(:standard-frame t
                  :repeat-enabled nil
                  :broadcast-type ,+broadcast-type-normal+
                  :priority ,+priority-low+
                  :ack-requested nil
                  :error-confirmation nil)
                (ctrl1-rep cut)))
    (is (equal (cemi-ctrl2 cut) #*11100000))
    (is (equalp `(:address-type knx-group-address
                  :hop-count 6
                  :frame-format 0)
                (ctrl2-rep cut)))
    (is (typep (cemi-source-addr cut) 'knx-individual-address))
    (is (typep (cemi-destination-addr cut) 'knx-group-address))
    (is (string= (address-string-rep
                  (cemi-source-addr cut)) "14.14.255"))
    (is (string= (address-string-rep
                  (cemi-destination-addr cut)) "0/0/4")) ; Light_EG_Az
    (is (equalp (cemi-tpci cut) +tcpi-udt+))
    (is (= (cemi-packet-num cut) 0))
    (is (apci-gv-read-p (cemi-apci cut)))
    (is (eq nil (cemi-data cut)))))

(defparameter *l-data.ind-gv-response*
  #(41 0 188 208 19 1 0 4 1 0 64 0 0 0)
  "Response to .req request.")

(test parse-l-data.ind-gv-response
  (let ((cut (parse-cemi *l-data.ind-gv-response*)))
    (print cut)
    (is (typep cut 'cemi-l-data))
    (is (= (cemi-message-code cut) +cemi-mc-l_data.ind+))
    
    (is (equal (cemi-ctrl1 cut) #*10111100))
    (is (equalp `(:standard-frame t
                  :repeat-enabled nil
                  :broadcast-type ,+broadcast-type-normal+
                  :priority ,+priority-low+
                  :ack-requested nil
                  :error-confirmation nil)
                (ctrl1-rep cut)))
    (is (equal (cemi-ctrl2 cut) #*11010000))
    (is (equalp `(:address-type knx-group-address
                  :hop-count 5
                  :frame-format 0)
                (ctrl2-rep cut)))
    (is (typep (cemi-source-addr cut) 'knx-individual-address))
    (is (typep (cemi-destination-addr cut) 'knx-group-address))
    (is (string= (address-string-rep
                  (cemi-source-addr cut)) "1.3.1"))
    (is (string= (address-string-rep
                  (cemi-destination-addr cut)) "0/0/4")) ; Light_EG_Az
    (is (equalp (cemi-tpci cut) +tcpi-udt+))
    (is (= (cemi-packet-num cut) 0))
    (is (apci-gv-response-p (cemi-apci cut)))
    (is (equalp #(0) (cemi-data cut)))))

(test parse-raw-to-bytes-and-parse-again
  (let* ((cut (parse-cemi *l-data.ind-gv-write*))
         (cut-bytes (knxobj:to-byte-seq cut)))
    (print *l-data.ind-gv-write*)
    (print cut-bytes)
    (is (equalp *l-data.ind-gv-write* cut-bytes))
    (is (= (cemi-len cut) (length cut-bytes)))
    (let ((parsed-cut (parse-cemi cut-bytes)))
      (print cut)
      (print parsed-cut)
      (is (equalp cut parsed-cut)))))

(test make-cemi--default
  (let ((cemi (make-default-cemi
               :message-code +cemi-mc-l_data.req+
               :dest-address (make-group-address "0/4/10")
               :apci (make-apci-gv-write)
               :dpt (make-dpt1 :switch :on))))
    (is-true cemi)
    (is (typep cemi 'cemi-l-data))))

(test cemi-to-bytes--parse-from-bytes--dpt-1.001--optimized-data
  (let* ((cemi (make-default-cemi
                :message-code +cemi-mc-l_data.req+
                :dest-address (make-group-address "0/4/10")
                :apci (make-apci-gv-write)
                :dpt (make-dpt1 :switch :on)))
         (cemi-bytes (knxobj:to-byte-seq cemi)))
    (print cemi)
    (is (= (cemi-len cemi) (length cemi-bytes)))
    (let ((parsed-cemi (parse-cemi cemi-bytes)))
      (setf (cemi-data parsed-cemi)
            (parse-to-dpt 'dpt-1.001 (cemi-data parsed-cemi)))
      (is (equalp cemi parsed-cemi)))))

(test cemi-to-bytes--parse-from-bytes--dpt-9.001--non-optimized-data
  (let* ((cemi (make-default-cemi
                :message-code +cemi-mc-l_data.req+
                :dest-address (make-group-address "0/4/10")
                :apci (make-apci-gv-write)
                :dpt (make-dpt9 :temperature 23.5)))
         (cemi-bytes (knxobj:to-byte-seq cemi)))
    (print cemi)
    (is (= (cemi-len cemi) (length cemi-bytes)))
    (let ((parsed-cemi (parse-cemi cemi-bytes)))
      (setf (cemi-data parsed-cemi)
            (parse-to-dpt 'dpt-9.001 (cemi-data parsed-cemi)))
      (is (equalp cemi parsed-cemi)))))
