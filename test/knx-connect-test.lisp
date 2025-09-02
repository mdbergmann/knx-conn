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

(defvar *test-tunnelling-request-receive* nil)
(defvar *test-tunnelling-request-ack* nil)

(def-fixture request-value (receive-tunn-req-delay
                            connect-ok
                            &optional 
                            (allow-auto-ack t))
  (with-mocks ()
    (let ((response-to-receive))
      (answer ip-client:ip-connect
        (progn
          (setf ip-client::*conn* 'dummy)
          (setf ip-client:*local-host-and-port*
                (cons #(192 168 1 1) 1234))))
      (answer (ip-client:ip-send-knx-data req)
        (etypecase req
          (knx-connect-request
           (when connect-ok
             (setf response-to-receive
                   (make-test-connect-response))))
          (knx-tunnelling-request
           (setf response-to-receive
                 (or *test-tunnelling-request-receive*
                     (and allow-auto-ack
                          (make-tunnelling-ack-2 knx-conn.knx-client::*channel-id*
                                                 (tunnelling-seq-counter req))))))
          (knx-tunnelling-ack
           t)
          (knx-disconnect-request
           (setf response-to-receive
                 (make-disconnect-response 1 0)))))
      (answer ip-client:ip-receive-knx-data
        (if (null knx-client::*channel-id*)
            (typecase response-to-receive
              (knx-connect-response
               (progn
                 (setf knx-client::*channel-id* 1)
                 (prog1
                     `(,response-to-receive nil)
                   (setf response-to-receive nil)))))
            (when response-to-receive
              ;; if there is an ack serve it first
              (if *test-tunnelling-request-ack*
                  (prog1
                      `(,*test-tunnelling-request-ack* nil)
                    (setf *test-tunnelling-request-ack* nil))
                  (progn
                    (typecase response-to-receive
                      (knx-tunnelling-request
                       (sleep receive-tunn-req-delay))
                      (knx-disconnect-response
                       (setf knx-client::*channel-id* nil)))
                    (prog1
                        `(,response-to-receive nil)
                      (setf response-to-receive nil)))))))
      (answer ip-client:ip-disconnect
        (setf ip-client::*conn* nil)
        (setf ip-client:*local-host-and-port* nil))    
    
      (setf knx-client::*receive-knx-data-recur-delay-secs* 0.5)
      (unwind-protect
           (progn
             (&body))
        (setf *test-tunnelling-request-receive* nil
              *test-tunnelling-request-ack* nil
              knx-client::*tunnel-ack-wait-timeout-secs*
              knx-client::*default-response-wait-timeout-secs*)))))

;; --------------------------------------
;; initialize
;; --------------------------------------

(test init-destroy--ensure-connection-asys-and-tunnel
  (with-fixture request-value (0 t)
    (unwind-protect
         (progn
           (knx-conn-init "123.23.45.21"
                          :enable-heartbeat nil)
           (is-true (await-cond 1.5
                      (eq ip-client::*conn* 'dummy)))
           (is-true (await-cond 1.5
                      (not (null knxc::*asys*))))
           (is-true (await-cond 1.5
                      (eql knx-client::*channel-id* 1))))
      (knx-conn-destroy))
    (is-true (await-cond 1.5
               (= (length (invocations
                           'ip-client:ip-connect)) 1)))
    (is-true (await-cond 1.5
               (= (length (invocations
                           'ip-client:ip-send-knx-data)) 2)))
    (is-true (await-cond 1.5
               (>= (length (invocations
                            'ip-client:ip-receive-knx-data)) 1)))
    (is-true (await-cond 1.5
               (= (length (invocations
                           'ip-client:ip-disconnect)) 1)))
    ;; check cleanup
    (is (eql ip-client::*conn* nil))
    (is (eql knxc::*asys* nil))
    (is (eql knx-client::*channel-id* nil))
    (is (eql knx-client:*receive-knx-data-recur-delay-secs* 0))
    (is (eql knx-client::*tunnel-request-listeners* nil))
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
    (answer ip-client:ip-connect
      (setf ip-client::*local-host-and-port*
            (cons #(192 168 1 1) 1234)))
    (answer ip-client:ip-receive-knx-data
      `(,(make-test-connect-response) nil))
    (answer ip-client:ip-connected-p t)
    (answer ip-client:ip-send-knx-data t)
    (answer knx-client:tunnel-connection-established-p t)
    (answer knx-client:send-connection-state t)
    (unwind-protect
         (let ((knx-client::*default-receive-knx-data-recur-delay-secs*
                 1.0)
               (knx-client::*default-heartbeat-interval-secs* .5))
           (knx-conn-init "12.23.34.45"
                          :enable-heartbeat t)
           (is-true (await-cond 1.0
                      (>= (length (invocations
                                   'knx-client:send-connection-state)) 1))))
      (knx-conn-destroy))))

;; ------------------------------------
;; with-knx/ip macro
;; ------------------------------------

(defun make-test-tunnelling-ack (&optional (seq-id 0))
  (tunnelling:make-tunnelling-ack-2 1 seq-id))

;; write-value
;; -----------

(test with-knx/ip--write-value--ok
  (setf *test-tunnelling-request-receive* (make-test-tunnelling-ack))
  (with-fixture request-value (0 t)
    (with-knx/ip ("12.23.34.45" :port 1234)
      (is (eq t
              (fawait
               (write-value "1/2/3"
                            'dpt:dpt-1.001
                            t)
               :timeout 5))))
    (is-true (await-cond 1.5
               (= (length (invocations
                           'ip-client:ip-connect)) 1)))
    (is-true (await-cond 1.5
               (>= (length (invocations
                            'ip-client:ip-receive-knx-data)) 1)))
    ;; connect, write, disconnect
    (is-true (await-cond 1.5
               (>= (length (invocations
                            'ip-client:ip-send-knx-data)) 3)))
    (is-true (await-cond 1.5
               (= (length (invocations
                           'ip-client:ip-disconnect)) 1)))

    (is-false ip-client::*conn*)
    (is-false knx-client::*channel-id*)))

(test with-knx/ip--write-value--supported-dpt-types
  (with-fixture request-value (0 t)
    (with-knx/ip ("12.23.34.45" :port 1234)
      (let ((dpts `((dpt:dpt-1.001 . nil)
                    (dpt:dpt-5.001 . 54)
                    (dpt:dpt-5.010 . 123)
                    (dpt:dpt-9.001 . 23.5)
                    (dpt:dpt-10.001 . ,(local-time:now))
                    (dpt:dpt-11.001 . ,(local-time:today))
                    )))
        (mapcar (lambda (dpt-vals)
                  (let ((dpt-type (car dpt-vals))
                        (val (cdr dpt-vals)))
                    (is-true (fawait
                              (write-value "1/2/3" dpt-type val)
                              :timeout 5))))
                dpts)))))

(test with-knx/ip--write-value--err-no-ack
  (with-fixture request-value (0 t nil)
    (with-knx/ip ("12.23.34.45" :port 1234)
      (setf knx-client::*tunnel-ack-wait-timeout-secs* 1)
      (ignore-errors
       (is (typep 
            (fawait
             (write-value "1/2/3"
                          'dpt:dpt-1.001
                          t)
             :timeout 5)
            'knx-client:knx-response-timeout-error))))
    (is-true (await-cond 1.5
               (= (length (invocations
                           'ip-client:ip-connect)) 1)))
    (is-true (await-cond 1.5
               (>= (length (invocations
                            'ip-client:ip-receive-knx-data)) 1)))
    ;; connect, write, disconnect
    (is-true (await-cond 1.5
               (>= (length (invocations
                            'ip-client:ip-send-knx-data)) 3)))
    (is-true (await-cond 1.5
               (= (length (invocations
                           'ip-client:ip-disconnect)) 1)))

    (is-false ip-client::*conn*)
    (is-false knx-client::*channel-id*)))

(test with-knx/ip--error-on-ip-connect-should-cleanup--crit-error
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

(test with-knx/ip--error-on-establish-tunnel-connection--crit-error
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
               (= (length (invocations
                           'ip-client:ip-connect)) 1)))
    (is-true (await-cond 1.5
               (>= (length (invocations
                            'ip-client:ip-receive-knx-data)) 1)))
    (is-true (await-cond 1.5
               (= (length (invocations
                           'ip-client:ip-send-knx-data)) 1)))
    (is-true (await-cond 1.5
               (= (length (invocations
                           'ip-client:ip-disconnect)) 1)))))

;; request-value
;; -------------

(defun make-test-tunnel-request-dpt-1.001-ind_response ()
  (tunnelling:make-tunnelling-request
   :channel-id 1
   :seq-counter 1
   :cemi (cemi:make-default-cemi
          :message-code +cemi-mc-l_data.ind+
          :dest-address (address:make-group-address "1/2/3")
          :apci (make-apci-gv-response)
          :dpt (dpt:make-dpt1 :switch :on))))

(test request-value--wait-for-value--ok
  (setf *test-tunnelling-request-ack*
        (make-test-tunnelling-ack))
  (setf *test-tunnelling-request-receive*
        (make-test-tunnel-request-dpt-1.001-ind_response))
  (with-fixture request-value (0 t)
    (with-knx/ip ("12.23.34.45")
      (let ((value
              (fawait (request-value "1/2/3" 'dpt:dpt-1.001)
                      :timeout 10.0)))
        (is (eq value :on))
        ))))

(test request-value--wait-for-value--ok--with-additional-ga-dpt-mapping
  (setf *test-tunnelling-request-ack*
        (make-test-tunnelling-ack))
  (setf *test-tunnelling-request-receive*
        (parse-root-knx-object
         (to-byte-seq
          (make-test-tunnel-request-dpt-1.001-ind_response))))
  (with-fixture request-value (0 t)
    (with-knx/ip ("12.23.34.45")
      (setf knx-client:*group-address-dpt-mapping*
            '(("1/2/3" dpt:dpt-1.001 "Foobar")))
      (let ((value
              (fawait (request-value "1/2/3" 'dpt:dpt-1.001)
                      :timeout 10.0)))
        (is (eq value :on))
        ))))

(test request-value--wait-for-value--timeout
  (setf *test-tunnelling-request-receive*
        (make-test-tunnel-request-dpt-1.001-ind_response))
  (with-fixture request-value (1.5 t)
    (with-knx/ip ("12.23.34.45")
      (multiple-value-bind (res fut)
          (fawait (request-value "1/2/3" 'dpt:dpt-1.001)
                  :timeout 1.0)
        (is (not (null fut)))
        (is (eq :on res))
        ))))

(defun make-test-tunnel-request-dpt-9.001-ind_response ()
  (knx-conn.knx-obj:parse-root-knx-object
   (to-byte-seq
    (tunnelling:make-tunnelling-request
     :channel-id 1
     :seq-counter 1
     :cemi (cemi:make-default-cemi
            :message-code +cemi-mc-l_data.ind+
            :dest-address (address:make-group-address "1/2/3")
            :apci (make-apci-gv-response)
            :dpt (dpt:make-dpt9 :temperature 23.5))))))

(test request-value--wait-for-value--unable-to-parse-dpt-type
  ;; dpt-type 9.001
  (setf *test-tunnelling-request-ack*
        (make-test-tunnelling-ack))
  (setf *test-tunnelling-request-receive*
        (make-test-tunnel-request-dpt-9.001-ind_response))
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
