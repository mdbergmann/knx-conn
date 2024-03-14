(defpackage :knx-conn.descr-info-test
  (:use :cl :fiveam :knx-conn.descr-info :knxobj :hpai)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :knx-conn.descr-info-test)

(def-suite descr-info-tests
  :description "Tests for description info"
  :in knx-conn.tests:test-suite)

(in-suite descr-info-tests)


(defparameter *raw-descr-request*
  (make-array 14
              :element-type '(unsigned-byte 8)
              :initial-contents
              '(#x06 #x10
                #x02 #x03
                #x00 #x0e
                ;; HPAI
                #x08
                #x01                ;; udp
                #x00 #x00 #x00 #x00 ;; unbound address
                #x00 #x00
                )))

(test descr-request--compare-to-raw
  (is (equalp *raw-descr-request*
              (to-byte-seq
               (make-descr-request hpai:*hpai-unbound-addr*)))))

(defparameter *raw-descr-request-2*
  (make-array 14
              :element-type '(unsigned-byte 8)
              :initial-contents
              '(#x06 #x10
                #x02 #x03
                #x00 #x0e
                ;; HPAI
                #x08
                #x01 ;; udp
                192 168 50 100
                195 180)))

(test descr-request--compare-to-raw-2
  (is (equalp *raw-descr-request-2*
              (to-byte-seq
               (make-descr-request
                (hpai:make-hpai "192.168.50.100" 50100))))))
