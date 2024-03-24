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

;; --------------------------------------
;; initialize
;; --------------------------------------

(test init-destroy--ensure-connection-and-asys
  (with-mocks ()
    (answer usocket:socket-connect 'dummy)
    (answer usocket:socket-close t)
    (unwind-protect
         (progn
           (setf ip-client::*conn* nil)
           (setf knxc::*asys* nil)
           (knx-conn-init "123.23.45.21"
                          :start-receive nil)
           (is (eq ip-client::*conn* 'dummy))
           (is (not (null knxc::*asys*))))
      (knx-conn-destroy))
    (is (= 1 (length (invocations 'usocket:socket-connect))))
    (is (= 1 (length (invocations 'usocket:socket-close))))
    (is (eq ip-client::*conn* nil))
    (is (eq knxc::*asys* nil))
    (is (eql knx-client:*receive-knx-data-recur-delay-secs* 0))
    (is (eq knx-client:*tunnel-request-listeners* nil))
    (is (eq knx-client:*async-handler* nil))))

(test init--no-connect-when-already-connected
  (let ((ip-client::*conn* 'dummy))
    (with-mocks ()
      (answer usocket:socket-connect 'new)
      (handler-case
          (progn
            (knx-conn-init "123.23.45.21")
            (fail "Should not connect again!"))
        (error (c)
          (is (equal (format nil "~a" c)
                     "Already connected!"))))
      (is (eq ip-client::*conn* 'dummy))
      (is (= 0 (length (invocations 'usocket:socket-connect)))))))

(test init--no-init-asys-when-already-initialized
  (let ((knxc::*asys* 'dummy)
        (ip-client::*conn* nil)
        (knx-client:*async-handler* 'dummy))
    (with-mocks ()
      (answer usocket:socket-connect 'new)
      (answer asys:make-actor-system 'foo)
      (knx-conn-init "123.23.45.21"
                     :start-receive nil)
      (is (eq knxc::*asys* 'dummy))
      (is (= 0 (length (invocations 'asys:make-actor-system)))))))    

(test init--start-async-receive--does-start-receive
  "Make sure `start-async-receive` starts the recurring receiving."
  (setf knxc::*asys* nil
        ip-client::*conn* nil)
  (with-mocks ()
    (answer usocket:socket-connect 'dummy)
    (answer usocket:socket-close t)
    (answer usocket:socket-receive #())
    (unwind-protect
         (progn
           ;; delay things a bit only
           (setf knx-client:*receive-knx-data-recur-delay-secs* .5)
           (knx-conn-init "12.23.34.45"
                          :start-receive t)
           (is-true (await-cond 1.5
                      (> (length (invocations 'usocket:socket-receive)) 1))))
      (progn
        (knx-conn-destroy)
        (setf knx-client:*receive-knx-data-recur-delay-secs* 0)))))

(test init--no-start-async-receive--does-not-start-receive
  (let ((knxc::*asys* nil)
        (ip-client::*conn* nil))
    (with-mocks ()
      (answer usocket:socket-connect 'dummy)
      (answer usocket:socket-close t)
      (answer usocket:socket-receive #())
      (unwind-protect
           (progn
             (knx-conn-init "12.23.34.45"
                            :start-receive nil)
             (sleep .5)
             (is (= 0 (length (invocations 'usocket:socket-receive)))))
        (knx-conn-destroy)))))

(test init--enable-heartbeat--does-start-heartbeat
  "Make sure `start-heartbeat` starts the recurring heartbeat."
  (setf knxc::*asys* nil
        ip-client::*conn* nil)
  (with-mocks ()
    (answer usocket:socket-connect 'dummy)
    (answer usocket:socket-close t)
    (answer usocket:socket-send t)
    (unwind-protect
         (progn
           ;; delay things a bit only
           (setf knx-client::*heartbeat-interval-secs* .5)
           (knx-conn-init "12.23.34.45"
                          :enable-heartbeat t
                          :start-receive nil)
           (is-true (await-cond 1.5
                      (> (length (invocations 'usocket:socket-send)) 1))))
      (progn
        (knx-conn-destroy)
        (setf knx-client::*heartbeat-interval-secs*
              knx-client::+default-heartbeat-interval-secs+)))))

(test with-knx/ip--write-value--ok
  (setf knxc::*asys* nil
        knx-client::*channel-id* nil
        ip-client::*conn* nil)
  (let ((connect-request-sent)
        (write-data-request-sent)
        (connect-response-received)
        (disconnect-request-sent))
    ;; slow things down, the mocks are not blocking
    (setf knx-client::*receive-knx-data-recur-delay-secs* 1.0)
    (with-mocks ()
      (answer ip-client:ip-connect
        (setf ip-client::*conn* 'dummy))
      (answer (ip-client:ip-send-knx-data req)
        (etypecase req
          (connect:knx-connect-request
           (setf connect-request-sent t))
          (tunnelling:knx-tunnelling-request
           (setf write-data-request-sent t))
          (connect:knx-disconnect-request
           (setf disconnect-request-sent t))
          ))
      (answer ip-client:ip-receive-knx-data
        (when (and connect-request-sent (not connect-response-received))
          (setf connect-response-received t)
          (let ((resp (connect::%make-connect-response
                       :header (make-header connect::+knx-connect-response+ 6)
                       :channel-id 1
                       :status 0
                       :hpai hpai::*hpai-unbound-addr*
                       :crd (crd::%make-crd
                             :conn-type
                             #x04
                             :individual-address
                             (address:make-individual-address "1.2.3")))))
            (format t "Returning connect response ~a~%" resp)
            `(,resp nil))))
      (answer ip-client:ip-disconnect
        (setf ip-client::*conn* nil))

      (with-knx/ip ("12.23.34.45" :port 1234)
        (write-value "1/2/3"
                     'dpt:dpt-1.001
                     t))

      (is-true (await-cond 1.5 connect-request-sent))
      (is-true (await-cond 1.5 connect-response-received))
      (is-true (await-cond 1.5 write-data-request-sent))
      (is-true (await-cond 1.5 disconnect-request-sent))

      (is-false ip-client::*conn*)
      (is-false knx-client::*channel-id*)
      
      (is (= (length (invocations 'ip-client:ip-connect)) 1))
      (is (>= (length (invocations 'ip-client:ip-receive-knx-data)) 1))
      ;; connect, write, disconnect
      (is (= (length (invocations 'ip-client:ip-send-knx-data)) 3))
      (is (= (length (invocations 'ip-client:ip-disconnect)) 1))
      )))

(test with-knx/ip--error-on-ip-connect-should-cleanup
  (setf knxc::*asys* nil
        knx-client::*channel-id* nil
        ip-client::*conn* nil)
  ;; slow things down, the mocks are not blocking
  (setf knx-client::*receive-knx-data-recur-delay-secs* 1.0)
  (with-mocks ()
    (answer ip-client:ip-connect nil)
    (answer ip-client:ip-receive-knx-data nil)
    (answer ip-client:ip-send-knx-data nil)
    (answer ip-client:ip-disconnect nil)

    (handler-case
        (progn
          (with-knx/ip ("12.23.34.45" :port 1234)
            (write-value "1/2/3"
                         'dpt:dpt-1.001
                         t))
          (fail "Should not get here!"))
      (error (c)
        (is (equal (format nil "~a" c)
                   "Could not connect to KNX/IP"))))

    (is (= (length (invocations 'ip-client:ip-connect)) 1)))
    (is (>= (length (invocations 'ip-client:ip-receive-knx-data)) 1))
    ;; error on ip level, no requests should go out.
    (is (= (length (invocations 'ip-client:ip-send-knx-data)) 0))
    (is (= (length (invocations 'ip-client:ip-disconnect)) 1)))

(test with-knx/ip--error-on-establish-tunnel-connection
  (setf knxc::*asys* nil
        knx-client::*channel-id* nil
        ip-client::*conn* nil)
  (let ((connect-request-sent)
        (write-data-request-sent)
        (connect-response-received)
        (disconnect-request-sent))
    ;; slow things down, the mocks are not blocking
    (setf knx-client::*receive-knx-data-recur-delay-secs* 1.0)
    (with-mocks ()
      (answer ip-client:ip-connect
        (setf ip-client::*conn* 'dummy))
      (answer (ip-client:ip-send-knx-data req)
        (etypecase req
          (connect:knx-connect-request
           (setf connect-request-sent t))))
      (answer ip-client:ip-receive-knx-data
        (when (and connect-request-sent (not connect-response-received))
          (setf connect-response-received t)
          (let ((resp (connect::%make-connect-response
                       :header (make-header connect::+knx-connect-response+ 6)
                       :channel-id 1
                       :status 99
                       :hpai hpai::*hpai-unbound-addr*
                       :crd (crd::%make-crd
                             :conn-type
                             #x04
                             :individual-address
                             (address:make-individual-address "1.2.3")))))
            (format t "Returning connect response ~a~%" resp)
            `(,resp nil))))
      (answer ip-client:ip-disconnect
        (setf ip-client::*conn* nil))

      (handler-case
          (progn
            (with-knx/ip ("12.23.34.45" :port 1234)
              (write-value "1/2/3"
                           'dpt:dpt-1.001
                           t))
            (fail "Should not get here!"))
        (error (c)
          (is (equal (format nil "~a" c)
                     "Could not establish tunnel connection!"))))

      (is-true (await-cond 1.5 connect-request-sent))
      (is-true (await-cond 1.5 connect-response-received))
      (is-false (await-cond 0.5 write-data-request-sent))
      (is-false (await-cond 0.5 disconnect-request-sent))
      
      (is (= (length (invocations 'ip-client:ip-connect)) 1))
      (is (>= (length (invocations 'ip-client:ip-receive-knx-data)) 1))
      ;; connect
      (is (= (length (invocations 'ip-client:ip-send-knx-data)) 1))
      (is (= (length (invocations 'ip-client:ip-disconnect)) 1)))))

;; TODO: test for when write fails, should cleanup
