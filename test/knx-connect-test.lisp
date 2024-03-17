(defpackage :knx-conn.knx-connect-test
  (:use :cl :cl-mock :fiveam
   :knxutil :knxobj :descr-info :connect :tunnelling
   :sento.miscutils :sento.future
   :crd :cemi :address :dib :dpt :knxc))

(in-package :knx-conn.knx-connect-test)

(def-suite knx-connect-tests
  :description "Tests for KNX connection handling"
  :in knx-conn.tests:test-suite)

(in-suite knx-connect-tests)

(log:config :debug)
(log:config '(sento) :warn)

(def-fixture env (listener-fun start-receive)
  (let ((resp-wait-timeout-store
          knxc::*resp-wait-timeout-secs*)
        (channel-id
          knxc::*channel-id*))
    ;; high value to not recur too much
    (setf knxc::*receive-knx-data-recur-delay-secs* .5)
    (with-mocks ()
      (answer usocket:socket-connect 'dummy)
      (answer usocket:socket-close t)      
      (unwind-protect
           (progn
             (knx-conn-init "12.23.34.45"
                            :start-receiving start-receive
                            :tunnel-request-listeners
                            (if listener-fun
                                (list listener-fun)
                                nil))
             (&body))
        (progn
          (knx-conn-destroy)
          (setf knxc::*resp-wait-timeout-secs* resp-wait-timeout-store)
          (setf knxc::*receive-knx-data-recur-delay-secs* 0)
          (setf knxc::*channel-id* channel-id)
          (setf knxc::*seq-counter* 0))))))

;; --------------------------------------
;; initialize
;; --------------------------------------

(test init-destroy--ensure-connection-and-asys
  (with-mocks ()
    (answer usocket:socket-connect 'dummy)
    (answer usocket:socket-close t)
    (unwind-protect
         (progn
           (setf knxc::*conn* nil)
           (setf knxc::*asys* nil)
           (knx-conn-init "123.23.45.21"
                          :start-receiving nil)
           (is (eq knxc::*conn* 'dummy))
           (is (not (null knxc::*asys*))))
      (knx-conn-destroy))
    (is (= 1 (length (invocations 'usocket:socket-connect))))
    (is (= 1 (length (invocations 'usocket:socket-close))))
    (is (eq knxc::*conn* nil))
    (is (eq knxc::*asys* nil))))

(test init--no-connect-when-already-connected
  (let ((knxc::*conn* 'dummy))
    (with-mocks ()
      (answer usocket:socket-connect 'new)
      (handler-case
          (progn
            (knx-conn-init "123.23.45.21")
            (fail "Should not connect again!"))
        (error (c)
          (is (equal (format nil "~a" c)
                     "Already connected!"))))
      (is (eq knxc::*conn* 'dummy))
      (is (= 0 (length (invocations 'usocket:socket-connect)))))))

(test init--no-init-asys-when-already-initialized
  (let ((knxc::*asys* 'dummy)
        (knxc::*conn* nil))
    (with-mocks ()
      (answer usocket:socket-connect 'new)
      (answer asys:make-actor-system 'foo)
      (knx-conn-init "123.23.45.21"
                     :start-receiving nil)
      (is (eq knxc::*asys* 'dummy))
      (is (= 0 (length (invocations 'asys:make-actor-system)))))))    

(test init--start-async-receiving--does-start-receiving
  "Make sure `start-async-receiving` starts the recurring receiving."
  (setf knxc::*asys* nil
        knxc::*conn* nil)
  (with-mocks ()
    (answer usocket:socket-connect 'dummy)
    (answer usocket:socket-close t)
    (answer usocket:socket-receive #())
    (unwind-protect
         (progn
           (setf knxc::*receive-knx-data-recur-delay-secs* .5) ; delay things a bit only
           (knx-conn-init "12.23.34.45"
                          :start-receiving t)
           (is-true (await-cond 1.5
                      (> (length (invocations 'usocket:socket-receive)) 1))))
      (knx-conn-destroy))))

(test init--no-start-async-receiving--does-not-start-receiving
  (let ((knxc::*asys* nil)
        (knxc::*conn* nil))
    (with-mocks ()
      (answer usocket:socket-connect 'dummy)
      (answer usocket:socket-close t)
      (answer usocket:socket-receive #())
      (unwind-protect
           (progn
             (knx-conn-init "12.23.34.45"
                            :start-receiving nil)
             (sleep .5)
             (is (= 0 (length (invocations 'usocket:socket-receive)))))
        (knx-conn-destroy)))))

;; --------------------------------------
;; description request/response
;; --------------------------------------

(defparameter *descr-response-data*
  #(6 16 2 4 0 84 54 1 2 0 17 1 0 0 0 1 0 53 81 241 0 0 0 0 0 14 140 0 107 180 73
    80 32 73 110 116 101 114 102 97 99 101 32 78 49 52 56 0 0 0 0 0 0 0 0 0 0 0 0
    0 12 2 2 1 3 2 4 1 7 1 8 1 12 254 0 1 8 0 255 241 115 255 148 75 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))

(test retrieve-descr-info--request-response--check-package
  (with-fixture env (nil t)
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
  (with-fixture env (nil t)
    (answer usocket:socket-send t)
    (answer usocket:socket-receive (progn
                                     (sleep 2.0)
                                     nil))

    (setf knxc::*resp-wait-timeout-secs* 1)
    (let ((result-fut (retrieve-descr-info)))
      (sleep 2.0)
      (destructuring-bind (result err)
          (fawait result-fut :timeout 1)
        (is (null result))
        (is (typep err 'knx-receive-error))
        (is (equal (format nil "~a" err)
                   (format nil "KNX receive error: Timeout waiting for response of type KNX-DESCR-RESPONSE")))))))

(test wait-for-response--error-on-response-parsing
  "This also returns `:timeout` because the response couldn't be parsed correctly
and so it is not possible to determine if it is the wanted response or not.
In case of this the log must be checked."
  (with-fixture env (nil t)
    (answer usocket:socket-send t)
    (answer usocket:socket-receive (error "foo"))

    (setf knxc::*resp-wait-timeout-secs* 0)
    (let ((result-fut (retrieve-descr-info)))
      (destructuring-bind (result err)
          (fawait result-fut :timeout 2)
        (is (null result))
        (is (typep err 'knx-receive-error))))))

;; --------------------------------------
;; connect request/response
;; --------------------------------------

(defparameter *connect-response-data-ok*
  #(6 16 2 6 0 20 78 0 8 1 0 0 0 0 0 0 4 4 238 255 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))

(test connect--request-response--ok
  (with-fixture env (nil t)
    (answer usocket:socket-send t)
    (answer usocket:socket-receive *connect-response-data-ok*)

    (let ((knxc::*channel-id* 0))
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
  (with-fixture env (nil t)
    (answer usocket:socket-send t)
    (answer usocket:socket-receive *connect-response-data-ok*)

    (setf knxc::*channel-id* -1)
    (establish-tunnel-connection)
    (is-true (await-cond 1.5
               (= knxc::*channel-id* 78)))
    
    (is (= (length (invocations 'usocket:socket-send)) 1))
    (is (>= (length (invocations 'usocket:socket-receive)) 1))))

(defparameter *connect-response-data-err*
  #(6 16 2 6 0 20 78 34 8 1 0 0 0 0 0 0 4 4 238 255 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
  "Connect response with error status")

(test connect--err
  (with-fixture env (nil t)
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
  (with-fixture env (nil t)
    (answer usocket:socket-send t)
    (answer usocket:socket-receive *connect-response-data-err*)

    (setf knxc::*channel-id* -1)
    (fawait (establish-tunnel-connection) :timeout 1)
    (is (= knxc::*channel-id* -1))
    
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

    (with-fixture env (nil t)
      (setf knxc::*channel-id* 78)
      (destructuring-bind (response err)
          (fawait (close-tunnel-connection) :timeout 1)
        (is (null err))
        (is (typep response 'knx-disconnect-response))
        (is (null knxc::*channel-id*)))
      
      (is (= (length (invocations 'usocket:socket-send)) 1))
      (is (>= (length (invocations 'usocket:socket-receive)) 1)))))

(test disconnect--err--no-valid-channel-id
  (with-fixture env (nil nil)
    (setf knxc::*conn* nil)
    (setf knxc::*channel-id* nil)
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

      (setf knxc::*channel-id* 78)
      (with-fixture env (nil t)
        (is-true (await-cond 1.5
                   (null knxc::*channel-id*))))

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

    (with-fixture env (nil t)
      (setf knxc::*channel-id* 78)
      (destructuring-bind (response err)
          (fawait (send-connection-state) :timeout 1)
        (is (null err))
        (is (typep response 'knx-connstate-response))))
      
    (is (= (length (invocations 'usocket:socket-send)) 1))
    (is (>= (length (invocations 'usocket:socket-receive)) 1))))

(test connection-state--err--no-valid-channel-id
  (with-fixture env (nil nil)
    (setf knxc::*conn* nil)
    (setf knxc::*channel-id* nil)
    (handler-case
        (send-connection-state)
      (simple-error (c)
        (is (equal (format nil "~a" c)
                   "No open connection!"))))
    (is (= (length (invocations 'usocket:socket-send)) 0))
    (is (= (length (invocations 'usocket:socket-receive)) 0))))

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
      (with-fixture env (listener-fun t)
        (await-cond 1.5
          (not (null request)))
        (is (not (null request)))
        (is (typep request 'knx-tunnelling-request)))
      (is (>= (length (invocations 'usocket:socket-receive)) 1)))))

;; --------------------------------------
;; tunneling request sending
;; --------------------------------------

(test send-write-request--switch-on
  (with-fixture env (nil nil)
    (answer usocket:socket-send t)

    (setf knxc::*channel-id* 78)
    (let ((req (send-write-request (make-group-address "0/4/10")
                                   (make-dpt1 :switch :on))))
      (is (= 78 (conn-header-channel-id
                 (tunnelling-request-conn-header req)))))
    (is-true (await-cond 0.5
               (= 1 (length (invocations 'usocket:socket-send)))))))

(test send-write-request--seq-counter--increment
  (with-fixture env (nil nil)
    (answer usocket:socket-send t)
    (setf knxc::*channel-id* 78)
    (setf knxc::*seq-counter* 0)
    (let ((req (send-write-request (make-group-address "0/4/10")
                                   (make-dpt1 :switch :on))))
      (is (= 1 (conn-header-seq-counter
                (tunnelling-request-conn-header req)))))
    (is (= 1 knxc::*seq-counter*))))

(test send-write-request--seq-counter--rollover
  (with-fixture env (nil nil)
    (answer usocket:socket-send t)
    (setf knxc::*channel-id* 78)
    (setf knxc::*seq-counter* 254)
    (let ((req (send-write-request (make-group-address "0/4/10")
                                   (make-dpt1 :switch :on))))
      (is (= 0 (conn-header-seq-counter
                (tunnelling-request-conn-header req)))))
    (is (= 0 knxc::*seq-counter*))))

(test send-read-request
  (with-fixture env (nil nil)
    (answer usocket:socket-send t)
    (setf knxc::*channel-id* 78)
    (setf knxc::*seq-counter* 79)
    (let ((req (send-read-request (make-group-address "0/4/10"))))
      (is (= 78 (conn-header-channel-id
                 (tunnelling-request-conn-header req))))
      (is (= 80 (conn-header-seq-counter
                 (tunnelling-request-conn-header req)))))
    (is-true (await-cond 0.5
               (= 1 (length (invocations 'usocket:socket-send)))))))
