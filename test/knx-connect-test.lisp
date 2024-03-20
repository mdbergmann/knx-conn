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
           (setf ip-client::*conn* nil)
           (setf knxc::*asys* nil)
           (knx-conn-init "123.23.45.21"
                          :start-receiving nil)
           (is (eq ip-client::*conn* 'dummy))
           (is (not (null knxc::*asys*))))
      (knx-conn-destroy))
    (is (= 1 (length (invocations 'usocket:socket-connect))))
    (is (= 1 (length (invocations 'usocket:socket-close))))
    (is (eq ip-client::*conn* nil))
    (is (eq knxc::*asys* nil))))

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
        (ip-client::*conn* nil))
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
        ip-client::*conn* nil)
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
        (ip-client::*conn* nil))
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
