(defpackage :knx-conn.knx-client-test
  (:use :cl :fiveam :cl-mock :knx-client
        :knxutil :knxobj :cemi :descr-info :connect :tunnelling
   :sento.miscutils :sento.future
   :crd :cemi :address :dib :dpt)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :knx-conn.knx-client-test)

(def-suite knx-client-tests
  :description "KNX client package tests"
  :in knx-conn.tests:test-suite)

(in-suite knx-client-tests)

(log:config :debug)
(log:config '(sento) :warn)

(def-fixture env (listener-fun start-receive)
  ;; store some variables to reset them after the test
  (let ((resp-wait-timeout-store
          *response-wait-timeout-secs*)
        (channel-id
          knx-client::*channel-id*)
        (test-asys (asys:make-actor-system
                    '(:dispatchers
                      (:shared (:workers 2)
                       :receiver (:workers 1)
                       :waiter (:workers 1))))))
    (with-mocks ()
      (answer ip-client:ip-connect
        (progn
          (setf ip-client::*conn* 'dummy)
          (setf ip-client:*local-host-and-port*
                (cons #(192 168 1 1) 3671))))
      (answer ip-client:ip-disconnect t)
      (unwind-protect
           (progn
             (ip-client:ip-connect "foo-bar")
             (reset-client-vars)
             ;; high value to not recur too much
             (setf *receive-knx-data-recur-delay-secs* .5)
             (make-async-handler test-asys)
             (when start-receive
               (start-async-receive))
             (when listener-fun
               (add-tunnelling-request-listener listener-fun))
             (&body))
        (progn
          (clr-tunnelling-request-listeners)
          (ignore-errors
           (ac:shutdown test-asys :wait t))
          (setf *async-handler* nil)
          (setf *response-wait-timeout-secs* resp-wait-timeout-store)
          (setf *receive-knx-data-recur-delay-secs* 0)
          (setf knx-client::*heartbeat-interval-secs*
                knx-client::*default-heartbeat-interval-secs*)
          (setf knx-client::*channel-id* channel-id)
          (setf knx-client::*seq-counter* 0)
          (setf ip-client::*conn* nil)
          (setf ip-client:*local-host-and-port* nil))))))

;; --------------------------------------
;; description request/response
;; --------------------------------------

(defparameter *test-descr-response*
  (parse-root-knx-object
   #(6 16 2 4 0 84 54 1 2 0 17 1 0 0 0 1 0 53 81 241 0 0 0 0 0 14 140 0 107 180 73
     80 32 73 110 116 101 114 102 97 99 101 32 78 49 52 56 0 0 0 0 0 0 0 0 0 0 0 0
     0 12 2 2 1 3 2 4 1 7 1 8 1 12 254 0 1 8 0 255 241 115 255 148 75 0 0 0 0 0 0)))

(test retrieve-descr-info--request-response--check-package
  (with-fixture env (nil t)
    (answer ip-client:ip-send-knx-data t)
    (answer ip-client:ip-receive-knx-data
      `(,*test-descr-response* nil))

    (let ((result-fut (retrieve-descr-info)))
      (await-cond 1.5
        (not (eq :not-ready (future:fresult result-fut))))
      (destructuring-bind (result err)
          (future:fresult result-fut)
        (is (eq nil err))
        (is (typep result 'knx-descr-response))
        ;; check knx-header
        (let ((header (package-header result)))
          (is (typep header 'knx-header))
          (is (eql knxobj::+knx-header-len+ (header-len header)))
          (is (eql descr-info::+knx-descr-response+ (header-type header)))
          (is (eql knxobj::+knx-netip-version+ (header-knxnetip-version header)))
          (is (eql (- 84 knxobj::+knx-header-len+) (header-body-len header))))
        ;; check dibs
        (is (typep (descr-response-device-hardware result)
                   'dib))
        (is (typep (descr-response-device-hardware result)
                   'dib-device-info))
        (is (typep (descr-response-supp-svc-families result)
                   'dib))
        (is (typep (descr-response-supp-svc-families result)
                   'dib-supp-svc-families))
        (is (typep (descr-response-other-dib-info result)
                   'dib-list))
        (is-false (endp (descr-response-other-dib-info result)))))
    
    (is (= (length (invocations 'ip-client:ip-send-knx-data)) 1))
    (is (>= (length (invocations 'ip-client:ip-receive-knx-data)) 1))))

(test wait-for-response--with-timeout--stop-when-elapsed
  "Test that when a response is expected but it doesn't come within the
   specified timeout, the result is timeout condition."
  (with-fixture env (nil t)
    (answer ip-client:ip-send-knx-data t)
    (answer ip-client:ip-receive-knx-data
      (progn
        (sleep 2.0) nil))
    (setf *response-wait-timeout-secs* 1)
    (let ((result-fut (retrieve-descr-info)))
      (sleep 2.0)
      (destructuring-bind (result err)
          (fawait result-fut :timeout 1)
        (is (null result))
        (is (typep err 'knx-response-timeout-error))
        (is (equal (format nil "~a" err)
                   (format nil "KNX timeout error: Timeout waiting for response of type KNX-DESCR-RESPONSE")))))))

(test wait-for-response--error-on-response-parsing
  "This also returns `:timeout` because the response couldn't be parsed correctly
and so it is not possible to determine if it is the wanted response or not.
In case of this the log must be checked."
  (with-fixture env (nil t)
    (answer ip-client:ip-send-knx-data t)
    (answer ip-client:ip-receive-knx-data
      `(nil ,(make-condition 'simple-error :format-control "foo")))

    (setf *response-wait-timeout-secs* 0)
    (let ((result-fut (retrieve-descr-info)))
      (destructuring-bind (result err)
          (fawait result-fut :timeout 2)
        (is (null result))
        (is (typep err 'knx-response-timeout-error))))))

;; --------------------------------------
;; connect request/response
;; --------------------------------------

(defparameter *test-connect-response--ok*
  (parse-root-knx-object
   #(6 16 2 6 0 20 78 0 8 1 0 0 0 0 0 0 4 4 238 255 0 0 0 0 0 0 0 0
     0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))

(test connect--request-response--ok
  (with-fixture env (nil t)
    (answer ip-client:ip-send-knx-data t)
    (answer ip-client:ip-receive-knx-data
      `(,*test-connect-response--ok* nil))

    ;; set channel-id to 0 to test that it is set to the value in the response
    (let ((knx-client::*channel-id* 0))
      (let ((result-fut (establish-tunnel-connection nil)))
        (destructuring-bind (response err)
            (fawait result-fut :timeout 1.5)
          (is (eq nil err))
          (is (typep response 'knx-connect-response))
          ;; check knx-header
          (let ((header (package-header response)))
            (is (typep header 'knx-header)))
          ;; check connect response body
          (is (eql +connect-status-no-error+ (connect-response-status response)))
          (is (eql 78 (connect-response-channel-id response)))
          (is (equal (address-string-rep
                      (crd:crd-individual-address
                       (connect-response-crd response)))
                     "14.14.255")))))
    
    (is (= (length (invocations 'ip-client:ip-send-knx-data)) 1))
    (is (>= (length (invocations 'ip-client:ip-receive-knx-data)) 1))))

(test connect--ok--sets-channel-id-and-seq-counter
  (with-fixture env (nil t)
    (answer ip-client:ip-send-knx-data t)
    (answer ip-client:ip-receive-knx-data
      `(,*test-connect-response--ok* nil))

    (setf knx-client::*channel-id* -1)
    (setf knx-client::*seq-counter* -1)
    (establish-tunnel-connection nil)
    (is-true (await-cond 1.5
               (and (= knx-client::*channel-id* 78)
                    (= knx-client::*seq-counter* 0))))
    
    (is (= (length (invocations 'ip-client:ip-send-knx-data)) 1))
    (is (>= (length (invocations 'ip-client:ip-receive-knx-data)) 1))))


(defparameter *test-connect-response--err*
  (parse-root-knx-object
   #(6 16 2 6 0 20 78 34 8 1 0 0 0 0 0 0 4 4 238 255 0 0 0 0 0 0 0))
  "Connect response with error status")

(test connect--err
  (with-fixture env (nil t)
    (answer ip-client:ip-send-knx-data t)
    (answer ip-client:ip-receive-knx-data
      `(,*test-connect-response--err* nil))

    (let ((response-fut (establish-tunnel-connection nil)))
      (destructuring-bind (resp err)
          (fawait response-fut :timeout 1.5)
        (is (null err))
        (is (= (connect-response-status resp)
               +connect-status-err-conn-type+))))

    (is (= (length (invocations 'ip-client:ip-send-knx-data)) 1))
    (is (>= (length (invocations 'ip-client:ip-receive-knx-data)) 1))))

(test connect--err--does-not-set-channel-id
  (with-fixture env (nil t)
    (answer ip-client:ip-send-knx-data t)
    (answer ip-client:ip-receive-knx-data
      `(,*test-connect-response--err* nil))

    (setf knx-client::*channel-id* -1)
    (fawait (establish-tunnel-connection nil) :timeout 1)
    (is (= knx-client::*channel-id* -1))
    
    (is (= (length (invocations 'ip-client:ip-send-knx-data)) 1))
    (is (>= (length (invocations 'ip-client:ip-receive-knx-data)) 1))))

(test connect--starts-heartbeat--ok
  (with-fixture env (nil t)
    (answer ip-client:ip-send-knx-data t)
    (answer ip-client:ip-receive-knx-data
      `(,*test-connect-response--ok* nil))
    (answer knx-client:send-connection-state t)

    (setf knx-client::*heartbeat-interval-secs* 1.0)
    (let ((response-fut (establish-tunnel-connection t)))
      (destructuring-bind (resp err)
          (fawait response-fut :timeout 1.5)
        (declare (ignore err))
        (is (= (connect-response-status resp)
               +connect-status-no-error+))))
    
    (is (= (length (invocations 'ip-client:ip-send-knx-data)) 1))
    (is (>= (length (invocations 'ip-client:ip-receive-knx-data)) 1))
    (is-true (await-cond 2.5
               (>= (length (invocations 'knx-client:send-connection-state)) 1)))))

;; --------------------------------------
;; disconnect request/response
;; --------------------------------------

(defparameter *test-disconnect-response--ok*
  (parse-root-knx-object #(6 16 2 10 0 8 0 0)))

(test disconnect--request-response--ok
  (with-mocks ()
    (answer ip-client:ip-send-knx-data t)
    (answer ip-client:ip-receive-knx-data
      `(,*test-disconnect-response--ok* nil))

    (with-fixture env (nil t)
      (setf knx-client::*channel-id* 78)
      (destructuring-bind (response err)
          (fawait (close-tunnel-connection) :timeout 1.5)
        (is (null err))
        (is (typep response 'knx-disconnect-response))
        (is (null knx-client::*channel-id*))))
      
    (is (= (length (invocations 'ip-client:ip-send-knx-data)) 1))
    (is (>= (length (invocations 'ip-client:ip-receive-knx-data)) 1))))

(test disconnect--err--no-valid-channel-id
  (with-fixture env (nil nil)
    (let ((knx-client::*channel-id* nil))
      (handler-case
          (close-tunnel-connection)
        (simple-error (c)
          (is (equal (format nil "~a" c)
                     "No open connection!"))))
    (is (= 0 (length (invocations 'ip-client:ip-send-knx-data))))
    (is (= 0 (length (invocations 'ip-client:ip-receive-knx-data)))))))

(test disconnect--received-request--closes-connection
  (with-mocks ()
    (let ((req (make-disconnect-request
                78 (cons #(12 23 34 45) 3671))))
      (answer ip-client:ip-receive-knx-data `(,req nil))

      (setf knx-client::*channel-id* 78)
      (with-fixture env (nil t)
        (is-true (await-cond 1.5
                   (null knx-client::*channel-id*))))

      (is (>= (length (invocations 'ip-client:ip-receive-knx-data)) 1)))))

;; --------------------------------------
;; connection-state request/response
;; --------------------------------------

(defparameter *test-connstate-response--ok*
  (parse-root-knx-object #(6 16 2 8 0 8 0 0)))

(test connection-state-request-response--ok
  (with-mocks ()
    (answer ip-client:ip-send-knx-data t)
    (answer ip-client:ip-receive-knx-data
      `(,*test-connstate-response--ok* nil))

    (with-fixture env (nil t)
      (setf knx-client::*channel-id* 78)
      (destructuring-bind (response err)
          (fawait (send-connection-state) :timeout 1.5)
        (is (null err))
        (is (typep response 'knx-connstate-response))))
      
    (is (= (length (invocations 'ip-client:ip-send-knx-data)) 1))
    (is (>= (length (invocations 'ip-client:ip-receive-knx-data)) 1))))

(test connection-state--err--no-valid-channel-id
  (with-fixture env (nil nil)
    (let ((knx-client::*channel-id* nil))
      (handler-case
          (send-connection-state)
        (simple-error (c)
          (is (equal (format nil "~a" c)
                     "No open connection!")))))
    (is (= (length (invocations 'ip-client:ip-send-knx-data)) 0))
    (is (= (length (invocations 'ip-client:ip-receive-knx-data)) 0))))

;; --------------------------------------
;; tunneling request receival
;; --------------------------------------

(defparameter *test-tunnelling-request--ok*
  (parse-root-knx-object
   #(6 16 4 32 0 23 4 76 0 0 41 0 188 208 19 14 4 10 3 0 128 12 104)))

(test tunnelling-receive-request--ok--is-delivered-to-listener ()
  (let* ((request)
         (listener-fun (lambda (req)
                         (setf request req))))
    (with-mocks ()
      (answer ip-client:ip-send-knx-data t)
      (answer ip-client:ip-receive-knx-data
        `(,*test-tunnelling-request--ok* nil))
      (with-fixture env (listener-fun t)
        (await-cond 1.5
          (not (null request)))
        (is (not (null request)))
        (is (typep request 'knx-tunnelling-request))
        (is (typep (cemi-data (tunnelling-request-cemi request)) 'array)))
      (is (>= (length (invocations 'ip-client:ip-receive-knx-data)) 1)))))

(defun make-test-tunnelling-req-1.001 ()
  ;; to bytes and back to have dpt as bytes
  (parse-root-knx-object
   (to-byte-seq (make-tunnelling-request
                 :channel-id 78
                 :seq-counter 0
                 :cemi (make-default-cemi
                        :message-code +cemi-mc-l_data.ind+
                        :dest-address (make-group-address "0/4/10")
                        :apci (make-apci-gv-write)
                        :dpt (make-dpt1 :switch :on))))))

(test tunnelling-receive-request--ok--parse-registered-dpt
  "Parses the dpt based on the provided ga -> dpt mapping."
  (let* ((req (make-test-tunnelling-req-1.001))
         (request)
         (listener-fun (lambda (req)
                         (setf request req))))
    (with-mocks ()
      (answer ip-client:ip-send-knx-data t)
      (answer ip-client:ip-receive-knx-data `(,req nil))
      (with-fixture env (listener-fun t)
        (setf knx-client:*group-address-dpt-mapping*
              '(("0/4/10" dpt:dpt-1.001 "My switch")))
        (is-true (await-cond 1.5
                   (not (null request))))
        (is (dpt:dpt-p (cemi-data (tunnelling-request-cemi request))))
        (is (dpt:dpt1-p (cemi-data (tunnelling-request-cemi request))))))))

(test tunnelling-received-ind-request-should-send-ack
  (let ((req (make-test-tunnelling-req-1.001)))
    (with-mocks ()
      (answer ip-client:ip-receive-knx-data `(,req nil))
      (answer (ip-client:ip-send-knx-data to-send)
        (progn
          (assert (typep to-send 'knx-tunnelling-ack) nil "Not a tunnelling ack!")
          (assert (= 78 (tunnelling-channel-id to-send)) nil "Wrong channel-id!")
          (assert (= 0 (tunnelling-seq-counter to-send)) nil "Wrong seq-counter!")))
      (with-fixture env (nil t)
        (is-true (await-cond 1.5
                   (>= (length (invocations 'ip-client:ip-receive-knx-data)) 1)))
        (is-true (await-cond 1.5
                   (>= (length (invocations 'ip-client:ip-send-knx-data)) 1)))))))

(test tunnelling-received-con-request-should-send-ack
  (let ((req (make-test-tunnelling-req-1.001)))
    (with-mocks ()
      (answer ip-client:ip-receive-knx-data `(,req nil))
      (answer (ip-client:ip-send-knx-data to-send)
        (progn
          (assert (typep to-send 'knx-tunnelling-ack) nil "Not a tunnelling ack!")
          (assert (= 78 (tunnelling-channel-id to-send)) nil "Wrong channel-id!")
          (assert (= 0 (tunnelling-seq-counter to-send)) nil "Wrong seq-counter!")))
      (with-fixture env (nil t)
        (is-true (await-cond 1.5
                   (>= (length (invocations 'ip-client:ip-receive-knx-data)) 1)))
        (is-true (await-cond 1.5
                   (>= (length (invocations 'ip-client:ip-send-knx-data)) 1)))))))

;; --------------------------------------
;; tunneling request sending
;; --------------------------------------

(test send-write-request--returns-future--resolves-with-ack
  (with-fixture env (nil t)
    (setf *receive-knx-data-recur-delay-secs* .1)
    (setf knx-client::*channel-id* 78)
    (answer ip-client:ip-send-knx-data t)
    (answer ip-client:ip-receive-knx-data
      `(,(make-tunnelling-ack-2 78 0) nil))
    (destructuring-bind (ack _err)
        (fawait
         (send-write-request (make-group-address "0/4/10")
                             (make-dpt1 :switch :on))
         :timeout 1.0)
      (declare (ignore _err))
      (is (typep ack 'knx-tunnelling-ack))
      (is (= 78 (tunnelling-channel-id ack)))
      (is (= 0 (tunnelling-seq-counter ack))))
    (is-true (await-cond 1.5
               (= (length (invocations 'ip-client:ip-send-knx-data)) 1)))
    (is-true (await-cond 1.5
               (>= (length (invocations 'ip-client:ip-receive-knx-data)) 1)))))

(test send-write-request--seq-counter--increment
  (with-fixture env (nil nil)
    (setf knx-client::*channel-id* 78)
    (setf knx-client::*seq-counter* 0)
    (let ((seq-counters nil))
      ;; attention: we're mocking `act:!` here.
      ;; but it is also used in the fixture cleanup-code, so we have to have
      ;; `CALL-PREVIOUS` here to do that.
      (answer (act:! _ msg)
        (if (eq :send (car msg))
            (push (tunnelling-seq-counter (cdr msg)) seq-counters)
            (call-previous)))
      (answer act:? (with-fut t))
      (send-write-request (make-group-address "0/4/10")
                          (make-dpt1 :switch :on))
      (send-write-request (make-group-address "0/4/10")
                          (make-dpt1 :switch :on))
      (is (= (length seq-counters) 2))
      (is (= (second seq-counters) 0))
      (is (= (first seq-counters) 1)))))

(test send-write-request--seq-counter--rollover
  (with-fixture env (nil nil)
    (setf knx-client::*channel-id* 78)
    (setf knx-client::*seq-counter* 254)
    (let ((seq-counters nil))
      ;; attention: we're mocking `act:!` here.
      ;; but it is also used in the fixture cleanup-code, so we have to have
      ;; `CALL-PREVIOUS` here to do that.
      (answer (act:! _ msg)
        (if (eq :send (car msg))
            (push (tunnelling-seq-counter (cdr msg)) seq-counters)
            (call-previous)))
      (answer act:? (with-fut t))
      (send-write-request (make-group-address "0/4/10")
                          (make-dpt1 :switch :on))
      (send-write-request (make-group-address "0/4/10")
                          (make-dpt1 :switch :on))
      (is (= (length seq-counters) 2))
      (is (= (second seq-counters) 254))
      (is (= (first seq-counters) 0)))))

(test send-write-request--ack-timeout--should-repeat-send-once
  (with-fixture env (nil t)
    (setf knx-client::*channel-id* 78)
    (answer ip-client:ip-send-knx-data t)
    (answer ip-client:ip-receive-knx-data
      (progn
        (sleep 1.0) nil))
    (let ((knx-client::*tunnel-ack-wait-timeout-secs* 1.0))
      (destructuring-bind (ack err)
          (fawait
           (send-write-request (make-group-address "0/4/10")
                               (make-dpt1 :switch :on))
           :timeout 5.0)
        (is (null ack))
        (is (typep err 'knx-response-timeout-error))
        (is (= 4 (length (invocations 'ip-client:ip-send-knx-data))))))))

(test send-read-request--resolves-with-ack
  (with-fixture env (nil t)
    (setf *receive-knx-data-recur-delay-secs* .1)
    (setf knx-client::*channel-id* 78)
    (answer ip-client:ip-send-knx-data t)
    (answer ip-client:ip-receive-knx-data
      `(,(make-tunnelling-ack-2 78 0) nil))
    (destructuring-bind (ack _err)
        (fawait
         (send-read-request (make-group-address "0/4/10"))
         :timeout 1.0)
      (declare (ignore _err))
      (is (typep ack 'knx-tunnelling-ack))
      (is (= 78 (tunnelling-channel-id ack)))
      (is (= 0 (tunnelling-seq-counter ack))))
    (is-true (await-cond 1.5
               (= (length (invocations 'ip-client:ip-send-knx-data)) 1)))
    (is-true (await-cond 1.5
               (>= (length (invocations 'ip-client:ip-receive-knx-data)) 1)))))

(test send-read-request--ack-timeout
  (with-fixture env (nil t)
    (setf knx-client::*channel-id* 78)
    (answer ip-client:ip-send-knx-data t)
    (answer ip-client:ip-receive-knx-data
      (progn
        (sleep 1.0) nil))
    (let ((knx-client::*tunnel-ack-wait-timeout-secs* 0.5))
      (destructuring-bind (ack err)
          (fawait
           (send-read-request (make-group-address "0/4/10"))
           :timeout 2.0)
        (is (null ack))
        (is (typep err 'knx-response-timeout-error))))))

(test send-read-request--ack-timeout--send-second-request--ok
  (with-fixture env (nil t)
    (setf *receive-knx-data-recur-delay-secs* 0.1)
    (setf knx-client::*channel-id* 78)
    (let ((send-count 0))
      (answer ip-client:ip-send-knx-data
        (incf send-count))
      (answer ip-client:ip-receive-knx-data
        (cond
          ((= send-count 1) (progn (sleep 1.0) nil))
          ((= send-count 2) `(,(make-tunnelling-ack-2 78 0) nil))))
      (let ((knx-client::*tunnel-ack-wait-timeout-secs* 1.0))
        (destructuring-bind (ack err)
            (fawait
             (send-read-request (make-group-address "0/4/10"))
             :timeout 5.0)
          (is (typep ack 'knx-tunnelling-ack))
          (is (null err))))
      (is-true (await-cond 5.0
                 (= (length (invocations 'ip-client:ip-send-knx-data)) 2))))))

(test send-tunnel-request--wait-for-ack--when-sending-new-request
  "As per KNX-IP spec, it is not allowed to send a new request before the
   previous one has not been acknowledged."
  (with-fixture env (nil t)
    (setf *receive-knx-data-recur-delay-secs* .2)
    (setf knx-client::*channel-id* 78)
    (let (responses ack1 ack2)
      (answer (ip-client:ip-send-knx-data req)
        (setf responses
              (append responses (list (make-tunnelling-ack req)))))
      (answer ip-client:ip-receive-knx-data
        (when responses
          (let ((resp (pop responses)))
            `(,resp nil))))
      (fcompleted
          (send-read-request (make-group-address "0/4/10"))
          (result)
        (setf ack1 result))
      (sleep 0.05)
      (fcompleted
          (send-read-request (make-group-address "0/4/10"))
          (result)
        (setf ack2 result))
      (is-true (await-cond 1.5
                 (and (not (null ack1))
                      (= 0 (tunnelling-seq-counter (car ack1))))))
      (is-true (await-cond 1.5
                 (and (not (null ack2))
                      (= 1 (tunnelling-seq-counter (car ack2)))))))))
