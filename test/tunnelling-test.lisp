(defpackage :knx-conn.tunnelling-test
  (:use :cl :fiveam :knx-conn.tunnelling :address :cemi)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :knx-conn.tunnelling-test)

(def-suite tunnelling-tests
  :description "Tests for tunnelling request"
  :in knx-conn.tests:test-suite)

(in-suite tunnelling-tests)

(defparameter *raw-tunnelling-request-data*
  #(6 16 4 32 0 23 4 76 0 0 41 0 188 208 19 14 4 10 3 0 128 12 104))

(test parse-tunnelling-request
  (let ((request (knxobj:parse-root-knx-object
                  *raw-tunnelling-request-data*)))
    (is (typep request 'knx-tunnelling-request))
    (let ((conn-header (tunnelling-request-conn-header request)))
      (is (= (conn-header-channel-id conn-header) 76))
      (is (= (conn-header-seq-counter conn-header) 0)))
    (let ((cemi (tunnelling-request-cemi request)))
      (is (typep cemi 'cemi-l-data))
      (is (= (cemi-message-code cemi) +cemi-mc-l_data.ind+)))))

(defparameter *raw-tunnelling-request-data-2*
  #(6 16 4 32 0 21 4 74 16 0 41 0 188 208 19 14 0 4 1 0 129)
  "Optimized data for a DPT-1.001")

(test parse-tunnelling-request-2
  (let ((request (knxobj:parse-root-knx-object
                  *raw-tunnelling-request-data-2*)))
    (is (typep request 'knx-tunnelling-request))
    (let ((conn-header (tunnelling-request-conn-header request)))
      (is (= (conn-header-channel-id conn-header) 74))
      (is (= (conn-header-seq-counter conn-header) 16)))
    (let ((cemi (tunnelling-request-cemi request)))
      (is (typep cemi 'cemi-l-data))
      (is (= (cemi-message-code cemi) +cemi-mc-l_data.ind+)))))

(test parse-then-to-bytes
  (let* ((request (knxobj:parse-root-knx-object
                   *raw-tunnelling-request-data*))
         (bytes-seq (knxobj:to-byte-seq request)))
    (is (= (length *raw-tunnelling-request-data*)
           (length bytes-seq)))
    (is (equalp *raw-tunnelling-request-data* bytes-seq))
    (let ((from-bytes (knxobj:parse-root-knx-object bytes-seq)))
      (is (equalp request from-bytes)))))

(test make-tunnelling-request--default
  (let ((req (make-tunnelling-request
              :channel-id 0
              :seq-counter 0
              :cemi (make-default-cemi
                     :message-code +cemi-mc-l_data.ind+
                     :dest-address (address:make-group-address "1/2/3")
                     :apci (make-apci-gv-write)
                     :dpt (dpt:make-dpt1 :switch :off)))))
    (is (not (null req)))))

(test from-to-byte-seq
  (let* ((req (make-tunnelling-request
               :channel-id 0
               :seq-counter 0
               :cemi (make-default-cemi
                      :message-code +cemi-mc-l_data.ind+
                      :dest-address (address:make-group-address "1/2/3")
                      :apci (make-apci-gv-write)
                      :dpt (dpt:make-dpt1 :switch :off))))
         (byte-seq (knxobj:to-byte-seq req)))
    (let ((req-from-bytes (knxobj:parse-root-knx-object byte-seq)))
      ;; replace data with to-bytes dpt to have it comparable
      (setf (cemi-data (tunnelling-request-cemi req))
            (knxutil:seq-to-array #(0) :arr-type 'vector))
      (is (equalp req req-from-bytes)))))

;; TODO: Add more tests
