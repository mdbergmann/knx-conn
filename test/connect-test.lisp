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
            (connect::disconnect-request-header req)) 16))))

(test disconnect-request--to-byte-seq
  (let* ((req (make-disconnect-request 0))
         (bytes (to-byte-seq req)))
    (is (vectorp bytes))
    (is (= (length bytes) 16))
    ))

(run! 'connect-tests)
