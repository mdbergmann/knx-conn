(defpackage :knx-conn.knx-client-test
  (:use :cl :fiveam :cl-mock :knx-client
        :knxutil :knxobj :descr-info :connect :tunnelling
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

(def-fixture env (listener-fun start-receive enable-heartbeat)
  ;; store some variables to reset them after the test
  (let ((resp-wait-timeout-store
          *resp-wait-timeout-secs*)
        (channel-id
          knx-client::*channel-id*)
        (test-asys (asys:make-actor-system
                    '(:dispatchers
                      (:shared (:workers 2)
                       :receiver (:workers 1)
                       :waiter (:workers 1))))))
    ;; high value to not recur too much
    (setf *receive-knx-data-recur-delay-secs* 1.0)
    (with-mocks ()
      (answer usocket:socket-connect 'dummy)
      (answer usocket:socket-close t)      
      (unwind-protect
           (progn
             (ip-client:ip-connect "foo-bar")
             (make-async-handler test-asys)
             (when start-receive
               (start-async-receive))
             (when enable-heartbeat
               (start-heartbeat))
             (when listener-fun
               (add-tunnelling-request-listener listener-fun))
             (&body))
        (progn
          (clr-tunnelling-request-listeners)
          (ac:shutdown test-asys :wait t)
          (setf *async-handler* nil)
          (setf *resp-wait-timeout-secs* resp-wait-timeout-store)
          (setf *receive-knx-data-recur-delay-secs* 0)
          (setf knx-client::*channel-id* channel-id)
          (setf knx-client::*seq-counter* 0)
          (setf ip-client::*conn* nil))))))

;; --------------------------------------
;; description request/response
;; --------------------------------------

(defparameter *descr-response-data*
  #(6 16 2 4 0 84 54 1 2 0 17 1 0 0 0 1 0 53 81 241 0 0 0 0 0 14 140 0 107 180 73
    80 32 73 110 116 101 114 102 97 99 101 32 78 49 52 56 0 0 0 0 0 0 0 0 0 0 0 0
    0 12 2 2 1 3 2 4 1 7 1 8 1 12 254 0 1 8 0 255 241 115 255 148 75 0 0 0 0 0 0))

(test retrieve-descr-info--request-response--check-package
  (with-fixture env (nil t nil)
    (answer usocket:socket-send t)
    (answer usocket:socket-receive *descr-response-data*)

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
    
    (is (= (length (invocations 'usocket:socket-send)) 1))
    (is (>= (length (invocations 'usocket:socket-receive)) 1))))


(test wait-for-response--with-timeout--stop-when-elapsed
  "Test that when a response is expected but it doesn't come within the
   specified timeout, the result is timeout condition."
  (with-fixture env (nil t nil)
    (answer usocket:socket-send t)
    (answer usocket:socket-receive (progn
                                     (sleep 2.0)
                                     nil))
    (setf *resp-wait-timeout-secs* 1)
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
  (with-fixture env (nil t nil)
    (answer usocket:socket-send t)
    (answer usocket:socket-receive (error "foo"))

    (setf *resp-wait-timeout-secs* 0)
    (let ((result-fut (retrieve-descr-info)))
      (destructuring-bind (result err)
          (fawait result-fut :timeout 2)
        (is (null result))
        (is (typep err 'knx-response-timeout-error))))))

;; --------------------------------------
;; connect request/response
;; --------------------------------------

(defparameter *connect-response-data-ok*
  #(6 16 2 6 0 20 78 0 8 1 0 0 0 0 0 0 4 4 238 255 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))

(test connect--request-response--ok
  (with-fixture env (nil t nil)
    (answer usocket:socket-send t)
    (answer usocket:socket-receive *connect-response-data-ok*)

    ;; set channel-id to 0 to test that it is set to the value in the response
    (let ((knx-client::*channel-id* 0))
      (let ((result-fut (establish-tunnel-connection)))
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
    
    (is (= (length (invocations 'usocket:socket-send)) 1))
    (is (>= (length (invocations 'usocket:socket-receive)) 1))))

(test connect--ok--sets-channel-id
  (with-fixture env (nil t nil)
    (answer usocket:socket-send t)
    (answer usocket:socket-receive *connect-response-data-ok*)

    (setf knx-client::*channel-id* -1)
    (establish-tunnel-connection)
    (is-true (await-cond 1.5
               (= knx-client::*channel-id* 78)))
    
    (is (= (length (invocations 'usocket:socket-send)) 1))
    (is (>= (length (invocations 'usocket:socket-receive)) 1))))


(defparameter *connect-response-data-err*
  #(6 16 2 6 0 20 78 34 8 1 0 0 0 0 0 0 4 4 238 255 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
  "Connect response with error status")

(test connect--err
  (with-fixture env (nil t nil)
    (answer usocket:socket-send t)
    (answer usocket:socket-receive *connect-response-data-err*)

    (let ((response-fut (establish-tunnel-connection)))
      (destructuring-bind (resp err)
          (fawait response-fut :timeout 1.5)
        (is (null err))
        (is (= (connect-response-status resp)
               +connect-status-err-conn-type+))))
    
    (is (= (length (invocations 'usocket:socket-send)) 1))
    (is (>= (length (invocations 'usocket:socket-receive)) 1))))

(test connect--err--does-not-set-channel-id
  (with-fixture env (nil t nil)
    (answer usocket:socket-send t)
    (answer usocket:socket-receive *connect-response-data-err*)

    (setf knx-client::*channel-id* -1)
    (fawait (establish-tunnel-connection) :timeout 1)
    (is (= knx-client::*channel-id* -1))
    
    (is (= (length (invocations 'usocket:socket-send)) 1))
    (is (>= (length (invocations 'usocket:socket-receive)) 1))))

;; --------------------------------------
;; disconnect request/response
;; --------------------------------------

(defparameter *disconnect-response-data-ok*
  #(6 16 2 10 0 8 0 0))

(test disconnect--request-response--ok
  (with-mocks ()
    (answer usocket:socket-send t)
    (answer usocket:socket-receive *disconnect-response-data-ok*)

    (with-fixture env (nil t nil)
      (setf knx-client::*channel-id* 78)
      (destructuring-bind (response err)
          (fawait (close-tunnel-connection) :timeout 1.5)
        (is (null err))
        (is (typep response 'knx-disconnect-response))
        (is (null knx-client::*channel-id*)))
      
      (is (= (length (invocations 'usocket:socket-send)) 1))
      (is (>= (length (invocations 'usocket:socket-receive)) 1)))))

(test disconnect--err--no-valid-channel-id
  (with-fixture env (nil nil nil)
    (setf ip-client::*conn* nil)
    (setf knx-client::*channel-id* nil)
    (handler-case
        (close-tunnel-connection)
      (simple-error (c)
        (is (equal (format nil "~a" c)
                   "No open connection!"))))
    (is (= 0 (length (invocations 'usocket:socket-send))))
    (is (= 0 (length (invocations 'usocket:socket-receive))))))

(test disconnect--received-request--closes-connection
  (with-mocks ()
    (let ((req-bytes (to-byte-seq
                      (make-disconnect-request 78))))
      (answer usocket:socket-receive req-bytes)

      (setf knx-client::*channel-id* 78)
      (with-fixture env (nil t nil)
        (is-true (await-cond 1.5
                   (null knx-client::*channel-id*))))

      (is (>= (length (invocations 'usocket:socket-receive)) 1)))))

;; --------------------------------------
;; connection-state request/response
;; --------------------------------------

(defparameter *connstate-response-data-ok*
  #(6 16 2 8 0 8 0 0))

(test connection-state-request-response--ok
  (with-mocks ()
    (answer usocket:socket-send t)
    (answer usocket:socket-receive *connstate-response-data-ok*)

    (with-fixture env (nil t nil)
      (setf knx-client::*channel-id* 78)
      (destructuring-bind (response err)
          (fawait (send-connection-state) :timeout 1.5)
        (is (null err))
        (is (typep response 'knx-connstate-response))))
      
    (is (= (length (invocations 'usocket:socket-send)) 1))
    (is (>= (length (invocations 'usocket:socket-receive)) 1))))

(test connection-state--err--no-valid-channel-id
  (with-fixture env (nil nil nil)
    (setf ip-client::*conn* nil)
    (setf knx-client::*channel-id* nil)
    (handler-case
        (send-connection-state)
      (simple-error (c)
        (is (equal (format nil "~a" c)
                   "No open connection!"))))
    (is (= (length (invocations 'usocket:socket-send)) 0))
    (is (= (length (invocations 'usocket:socket-receive)) 0))))

(test connection-state--send-as-heartbeat--ok
  (with-mocks ()
    (answer (ip-client:ip-send-knx-data req)
      (assert (typep req 'knx-connstate-request) nil "wrong request type")
      t)
    (let ((knx-client::*heartbeat-interval-secs* 1))
      (with-fixture env (nil nil t)
        (is-true (await-cond 1.5
                   (>= (length (invocations 'ip-client:ip-send-knx-data)) 1)))))))

;; --------------------------------------
;; tunneling request receival
;; --------------------------------------

(defparameter *raw-tunnelling-request-data*
  #(6 16 4 32 0 23 4 76 0 0 41 0 188 208 19 14 4 10 3 0 128 12 104))

(test tunnelling-receive-request--ok--is-delivered-to-listener ()
  (let* ((request)
         (listener-fun (lambda (req)
                         (setf request req))))
    (with-mocks ()
      (answer usocket:socket-receive *raw-tunnelling-request-data*)
      (with-fixture env (listener-fun t nil)
        (await-cond 1.5
          (not (null request)))
        (is (not (null request)))
        (is (typep request 'knx-tunnelling-request)))
      (is (>= (length (invocations 'usocket:socket-receive)) 1)))))

;; --------------------------------------
;; tunneling request sending
;; --------------------------------------

(test send-write-request--switch-on
  (with-fixture env (nil nil nil)
    (answer usocket:socket-send t)

    (setf knx-client::*channel-id* 78)
    (let ((req (send-write-request (make-group-address "0/4/10")
                                   (make-dpt1 :switch :on))))
      (is (= 78 (conn-header-channel-id
                 (tunnelling-request-conn-header req)))))
    (is-true (await-cond 0.5
               (= 1 (length (invocations 'usocket:socket-send)))))))

(test send-write-request--seq-counter--increment
  (with-fixture env (nil nil nil)
    (answer usocket:socket-send t)
    (setf knx-client::*channel-id* 78)
    (setf knx-client::*seq-counter* 0)
    (let ((req (send-write-request (make-group-address "0/4/10")
                                   (make-dpt1 :switch :on))))
      (is (= 1 (conn-header-seq-counter
                (tunnelling-request-conn-header req)))))
    (is (= 1 knx-client::*seq-counter*))))

(test send-write-request--seq-counter--rollover
  (with-fixture env (nil nil nil)
    (answer usocket:socket-send t)
    (setf knx-client::*channel-id* 78)
    (setf knx-client::*seq-counter* 254)
    (let ((req (send-write-request (make-group-address "0/4/10")
                                   (make-dpt1 :switch :on))))
      (is (= 0 (conn-header-seq-counter
                (tunnelling-request-conn-header req)))))
    (is (= 0 knx-client::*seq-counter*))))

(test send-read-request
  (with-fixture env (nil nil nil)
    (answer usocket:socket-send t)
    (setf knx-client::*channel-id* 78)
    (setf knx-client::*seq-counter* 79)
    (let ((req (send-read-request (make-group-address "0/4/10"))))
      (is (= 78 (conn-header-channel-id
                 (tunnelling-request-conn-header req))))
      (is (= 80 (conn-header-seq-counter
                 (tunnelling-request-conn-header req)))))
    (is-true (await-cond 0.5
               (= 1 (length (invocations 'usocket:socket-send)))))))
