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

(defun make-test-connect-response ()
  (connect::%make-connect-response
   :header (make-header connect::+knx-connect-response+ 6)
   :channel-id 1
   :status 0
   :hpai hpai::*hpai-unbound-addr*
   :crd (crd::%make-crd
         :conn-type
         #x04
         :individual-address
         (address:make-individual-address "1.2.3"))))

(def-fixture request-value (receive-tunn-req-delay
                            connect-ok)
  (with-mocks ()
    (let ((response-to-receive))
      (answer ip-client:ip-connect
        (setf ip-client::*conn* 'dummy))
      (answer (ip-client:ip-send-knx-data req)
        (etypecase req
          (connect:knx-connect-request
           (when connect-ok
             (setf knx-client::*channel-id* 1)
             (setf response-to-receive
                   (make-test-connect-response))))
          (tunnelling:knx-tunnelling-request
           (setf response-to-receive
                 *test-tunnelling-request*))
          (connect:knx-disconnect-request
           (setf knx-client::*channel-id* nil))))
      (answer ip-client:ip-receive-knx-data
        (when response-to-receive
          (when (typep response-to-receive 'tunnelling:knx-tunnelling-request)
            (sleep receive-tunn-req-delay))
          (prog1
              `(,response-to-receive nil)
            (setf response-to-receive nil))))
      (answer ip-client:ip-disconnect
        (setf ip-client::*conn* nil))    
    
      (setf knx-client::*receive-knx-data-recur-delay-secs* 1.0)
      (&body))))

;; --------------------------------------
;; initialize
;; --------------------------------------

(test init-destroy--ensure-connection-asys-and-tunnel
  (with-fixture request-value (0 t)
    (unwind-protect
         (progn
           (knx-conn-init "123.23.45.21"
                          :start-receive t
                          :enable-heartbeat nil)
           (is-true (await-cond 1.5
                      (eq ip-client::*conn* 'dummy)))
           (is-true (await-cond 1.5
                      (not (null knxc::*asys*))))
           (is-true (await-cond 1.5
                      (eql knx-client::*channel-id* 1))))
      (knx-conn-destroy))
    (is-true (await-cond 1.5
               (= (length (invocations 'ip-client:ip-connect)) 1)))
    (is-true (await-cond 1.5
               (= (length (invocations 'ip-client:ip-send-knx-data)) 2)))
    (is-true (await-cond 1.5
               (>= (length (invocations 'ip-client:ip-receive-knx-data)) 1)))
    (is-true (await-cond 1.5
               (= (length (invocations 'ip-client:ip-disconnect)) 1)))
    ;; check cleanup
    (is (eql ip-client::*conn* nil))
    (is (eql knxc::*asys* nil))
    (is (eql knx-client::*channel-id* nil))
    (is (eql knx-client:*receive-knx-data-recur-delay-secs* 0))
    (is (eql knx-client:*tunnel-request-listeners* nil))
    (is (eql knx-client:*async-handler* nil))))

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

(test init--enable-heartbeat--does-start-heartbeat
  "Make sure `start-heartbeat` starts the recurring heartbeat."
  (with-mocks ()
    (answer ip-client:ip-connect t)
    (answer ip-client:ip-connected-p t)
    (answer knx-client:establish-tunnel-connection (with-fut t))
    (answer knx-client:tunnel-connection-established-p t)
    (answer knx-client:send-connection-state t)
    (unwind-protect
         (progn
           ;; delay things a bit only
           (setf knx-client::*heartbeat-interval-secs* .5)
           (knx-conn-init "12.23.34.45"
                          :enable-heartbeat t
                          :start-receive nil)
           (is-true (await-cond 1.5
                      (> (length (invocations 'knx-client:send-connection-state)) 1))))
      (progn
        (knx-conn-destroy)
        (is (eql knx-client::*heartbeat-interval-secs*
                 knx-client::+default-heartbeat-interval-secs+))))))

(test with-knx/ip--write-value--ok
  (with-fixture request-value (0 t)
    (with-knx/ip ("12.23.34.45" :port 1234)
      (write-value "1/2/3"
                   'dpt:dpt-1.001
                   t))
    (is-true (await-cond 1.5
               (= (length (invocations 'ip-client:ip-connect)) 1)))
    (is-true (await-cond 1.5
               (>= (length (invocations 'ip-client:ip-receive-knx-data)) 1)))
    ;; connect, write, disconnect
    (is-true (await-cond 1.5
               (= (length (invocations 'ip-client:ip-send-knx-data)) 3)))
    (is-true (await-cond 1.5
               (= (length (invocations 'ip-client:ip-disconnect)) 1)))

    (is-false ip-client::*conn*)
    (is-false knx-client::*channel-id*)
    ))

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
    (is (= (length (invocations 'ip-client:ip-receive-knx-data)) 0))
    ;; error on ip level, no requests should go out.
    (is (= (length (invocations 'ip-client:ip-send-knx-data)) 0))
    (is (= (length (invocations 'ip-client:ip-disconnect)) 1)))

(test with-knx/ip--error-on-establish-tunnel-connection
  (with-fixture request-value (0 nil)
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
    (is-true (await-cond 1.5
               (= (length (invocations 'ip-client:ip-connect)) 1)))
    (is-true (await-cond 1.5
               (>= (length (invocations 'ip-client:ip-receive-knx-data)) 1)))
    (is-true (await-cond 1.5
               (= (length (invocations 'ip-client:ip-send-knx-data)) 1)))
    (is-true (await-cond 1.5
               (= (length (invocations 'ip-client:ip-disconnect)) 1)))))

(defun make-test-tunnelling-request ()
  (tunnelling:make-tunnelling-request
   :channel-id 1
   :seq-counter 1
   :cemi (cemi:make-default-cemi
          :message-code +cemi-mc-l_data.req+
          :dest-address (address:make-group-address "1/2/3")
          :apci (make-apci-gv-write)
          :dpt (dpt:make-dpt1 :switch :on))))

(defvar *test-tunnelling-request* nil)

(test request-value--wait-for-value
  (setf *test-tunnelling-request* (make-test-tunnelling-request))
  (with-fixture request-value (0 t)
    (with-knx/ip ("12.23.34.45")
      (let ((value (fawait (request-value "1/2/3" 'dpt:dpt-1.001)
                           :timeout 10.0)))
        (format t "value: ~a~%" value)
        (is (eq value :on))
        ))))

(test request-value--wait-for-value--timeout
  (setf *test-tunnelling-request* (make-test-tunnelling-request))
  (with-fixture request-value (1.5 t)
    (with-knx/ip ("12.23.34.45")
      (multiple-value-bind (res fut)
          (fawait (request-value "1/2/3" 'dpt:dpt-1.001)
                  :timeout 1.0)
        (format t "value: ~a~%" (list res fut))
        (is (null res))
        ))))

(defun make-test-tunnelling-request-from-bytes ()
  (knx-conn.knx-obj:parse-root-knx-object
   (to-byte-seq
    (tunnelling:make-tunnelling-request
     :channel-id 1
     :seq-counter 1
     :cemi (cemi:make-default-cemi
            :message-code +cemi-mc-l_data.req+
            :dest-address (address:make-group-address "1/2/3")
            :apci (make-apci-gv-write)
            :dpt (dpt:make-dpt9 :temperature 23.5))))))

(test request-value--wait-for-value--unable-to-parse-dpt-type
  ;; dpt-type 9.001
  (setf *test-tunnelling-request* (make-test-tunnelling-request-from-bytes))
  (with-fixture request-value (0 t)
    (with-knx/ip ("12.23.34.45")
      (multiple-value-bind (res fut)
          (fawait (request-value "1/2/3" 'dpt:dpt-1.001)
                  :timeout 10.0)
        (format t "value: ~a~%" (list res fut))
        (is (typep res 'knx-unable-to-parse))
        (is (equal (format nil "~a" res)
                   "Error condition: Byte vector must be of length 1, args: (DPT-1.001)"))
        ))))
