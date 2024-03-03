(defpackage :knx-conn.dpt-test
  (:use :cl :fiveam :knx-conn.dpt)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :knx-conn.dpt-test)

(def-suite dpt-tests
  :description "Tests for data point types"
  :in knx-conn.tests:test-suite)

(in-suite dpt-tests)

(test create-dpt1-1.001
  (let ((dpt (make-dpt1 :switch :on)))
    (is (= (dpt-raw-value dpt) 1))
    (is (eq (dpt-value dpt) :on))
    (is (equalp #(1) (knxobj:to-byte-seq dpt)))))

(run! 'dpt-tests)
