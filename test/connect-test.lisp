(defpackage :knx-conn.connect-test
  (:use :cl :fiveam :knx-conn.connect :knxobj :hpai)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :knx-conn.connect-test)

(def-suite connect-tests
  :description "Tunnel connect/disconnect"
  :in knx-conn.tests:test-suite)

(in-suite connect-tests)

(test make-disconnect-request--ok
  (let ((req (make-disconnect-request 0)))
    (is (typep req 'knx-disconnect-request))
    (is (= (connect::disconnect-request-channel-id req) 0))
    (is (equalp (connect::disconnect-request-hpai req)
                *hpai-unbound-addr*))
    (is (= (header-body-len
            (connect::disconnect-request-header req)) 10))))

(test disconnect-request--to-byte-seq
  (let* ((req (make-disconnect-request 0))
         (bytes (to-byte-seq req)))
    (is (vectorp bytes))
    (is (= (length bytes) 16))))

(test parse-disconnect-request
  (let ((req (parse-to-obj connect::+knx-disconnect-request+
                            (make-header connect::+knx-disconnect-request+ 2)
                            (concatenate '(vector octet)
                                         #(0 0)
                                         (to-byte-seq *hpai-unbound-addr*)))))
    (is (typep req 'knx-disconnect-request))
    (is (= (connect::disconnect-request-channel-id req) 0))))

(test parse-disconnect-response
  (let ((resp (parse-to-obj connect::+knx-disconnect-response+
                            (make-header connect::+knx-disconnect-response+ 2)
                            #(0 0))))
    (is (typep resp 'knx-disconnect-response))
    (is (= (connect::disconnect-response-channel-id resp) 0))
    (is (= (connect::disconnect-response-status resp) 0))))

(test make-connstate-request--ok
  (let ((req (make-connstate-request 0)))
    (is (typep req 'knx-connstate-request))
    (is (= connect::+knx-connstate-request+
           (header-type
            (connect::connstate-request-header req))))
    (is (= (connect::connstate-request-channel-id req) 0))
    (is (equalp (connect::connstate-request-hpai req)
                *hpai-unbound-addr*))
    (is (= (header-body-len
            (connect::connstate-request-header req)) 10))))

(test connstate-request--to-byte-seq
  (let* ((req (make-connstate-request 0))
         (bytes (to-byte-seq req)))
    (is (vectorp bytes))
    (is (= (length bytes) 16))))

(test parse-connstate-response
  (let ((resp (parse-to-obj connect::+knx-connstate-response+
                            (make-header connect::+knx-connstate-response+ 2)
                            #(0 0))))
    (is (typep resp 'knx-connstate-response))
    (is (= (connect::connstate-response-channel-id resp) 0))
    (is (= (connect::connstate-response-status resp) 0))))
