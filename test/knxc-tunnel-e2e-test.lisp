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
      (timeutils:wait-cond
       (lambda ()
         (every (lambda (x) (future:complete-p x)) changed))
       0.1 5)
      (format t "futs: ~a~%" (mapcar #'fresult changed))
    )))
