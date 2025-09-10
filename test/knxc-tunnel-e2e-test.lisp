(defpackage :knx-conn.knxc-tunnel-e2e-test
  (:use :cl :fiveam :knxc :future)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :knx-conn.knxc-tunnel-e2e-test)

(def-suite knxc-tunnel-e2e-tests
  :description "Manually run e2e tests against real KNX.")

(in-suite knxc-tunnel-e2e-tests)

;; run manually on demand

(test multiple-writes
  "Tests sending multiple, tightly sequenced writes."

  (with-knx/ip ("192.168.50.41")
    (let ((changed (list
                    ;; kitchen spots
                    (write-value "0/0/5" 'dpt:dpt-1.001 t)
                    ;; kitchen ambient
                    (write-value "0/0/9" 'dpt:dpt-1.001 t)
                    ;; kitchen 3
                    (write-value "0/0/10" 'dpt:dpt-1.001 t)
                    ;; az
                    (write-value "0/0/4" 'dpt:dpt-1.001 t))))
      (every (lambda (x) (eq t x)) changed)
      (format t "results: ~a~%" changed)
    )))

(test multiple-writes-with-heartbeat
  "Tests keeping the system running for longer having to send multiple heartbeats."

  (let ((knx-client::*default-heartbeat-interval-secs* 5))
    (knx-conn-init "192.168.50.41")
    (sleep 2)
    (let ((changed (list
                    ;; kitchen spots
                    (write-value "0/0/5" 'dpt:dpt-1.001 t)
                    ;; kitchen ambient
                    (write-value "0/0/9" 'dpt:dpt-1.001 t)
                    ;; kitchen 3
                    (write-value "0/0/10" 'dpt:dpt-1.001 t)
                    ;; az
                    (write-value "0/0/4" 'dpt:dpt-1.001 t))))
      (format t "results: ~a~%" changed)
      (is (every (lambda (x) (eq t x)) changed))
      )
    ;; letting it sleep for 1 minute to check sending heartbeats
    (sleep 60)
    (knx-conn-destroy)))
