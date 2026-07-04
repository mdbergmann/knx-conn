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
  (let ((req (make-disconnect-request 0 (cons #(127 0 0 1) 123))))
    (is (typep req 'knx-disconnect-request))
    (is (= (connect::disconnect-request-channel-id req) 0))
    (is (equalp (connect::disconnect-request-hpai req)
                (make-hpai #(127 0 0 1) 123)))
    (is (= (header-body-len
            (connect::disconnect-request-header req)) 10))))

(test disconnect-request--to-byte-seq
  (let* ((req (make-disconnect-request 0 (cons #(127 0 0 1) 123)))
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
    (is (= (disconnect-request-channel-id req) 0))))

(test parse-disconnect-response
  (let ((resp (parse-to-obj connect::+knx-disconnect-response+
                            (make-header connect::+knx-disconnect-response+ 2)
                            #(0 0))))
    (is (typep resp 'knx-disconnect-response))
    (is (= (disconnect-response-channel-id resp) 0))
    (is (= (disconnect-response-status resp) 0))))

(test disconnect-response--to-byte-seq
  (let* ((res (make-disconnect-response 1 0))
         (bytes (to-byte-seq res)))
    (is (vectorp bytes))
    (is (= (length bytes) 8))))

(test disconnect-response--to-from
  (let* ((res-bytes (to-byte-seq (make-disconnect-response 3 4)))
         (res (parse-root-knx-object res-bytes)))
    (is (typep res 'knx-disconnect-response))))

(test parse-connect-response--error-status--no-hpai-crd
  "An error connect-response is 8 bytes total: channel-id + status only (per
spec no data-endpoint HPAI and no CRD are included)."
  (let ((resp (parse-root-knx-object
               #(6 16 2 6 0 8 0 #x24))))
    (is (typep resp 'knx-connect-response))
    (is (= (connect-response-channel-id resp) 0))
    (is (= (connect-response-status resp)
           +connect-status-err-no-more-conns+))
    (is (null (connect::connect-response-hpai resp)))
    (is (null (connect-response-crd resp)))))

(test connect-status-name--decodes-spec-names
  (is (string= "NO_ERROR" (connect-status-name #x00)))
  (is (string= "E_CONNECTION_TYPE" (connect-status-name #x22)))
  (is (string= "E_NO_MORE_CONNECTIONS" (connect-status-name #x24)))
  (is (string= "UNKNOWN(#x42)" (connect-status-name #x42))))

(test parse-root-knx-object--too-short-for-header
  "Fewer bytes than a KNXnet/IP header must signal a clean parse condition."
  (signals knx-unable-to-parse
    (parse-root-knx-object #(2 6))))

(test parse-root-knx-object--declared-len-exceeds-buffer
  "A header declaring more payload than was actually received must signal a
clean parse condition instead of a subseq bounding error."
  (signals knx-unable-to-parse
    (parse-root-knx-object #(6 16 2 6 0 20 0 36))))

(test make-connstate-request--ok
  (let ((hpai (make-hpai #(127 0 0 1) 123))
        (req (make-connstate-request 0 (cons "127.0.0.1" 123))))
    (is (typep req 'knx-connstate-request))
    (is (= connect::+knx-connstate-request+
           (header-type
            (connect::connstate-request-header req))))
    (is (= (connect::connstate-request-channel-id req) 0))
    (is (equalp (connect::connstate-request-hpai req)
                hpai))
    (is (= (header-body-len
            (connect::connstate-request-header req)) 10))))

(test connstate-request--to-byte-seq
  (let* ((req (make-connstate-request 0 (cons "127.0.0.1" 123)))
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
