(defpackage :knx-conn.dpt-test
  (:use :cl :fiveam :knx-conn.dpt :knxobj)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :knx-conn.dpt-test)

(def-suite dpt-tests
  :description "Tests for data point types"
  :in knx-conn.tests:test-suite)

(in-suite dpt-tests)

(test create-dpt1-1.001
  ;; on
  (let ((dpt (make-dpt1 :switch :on)))
    (is (string= (dpt-value-type dpt) "1.001"))
    (is (= (dpt-raw-value dpt) 1))
    (is (eq (dpt-value dpt) :on))
    (is (equalp #(1) (knxobj:to-byte-seq dpt))))
  ;; off
  (let ((dpt (make-dpt1 :switch :off)))
    (is (= (dpt-raw-value dpt) 0))
    (is (eq (dpt-value dpt) :off))
    (is (equalp #(0) (to-byte-seq dpt))))
  ;; errors
  (signals type-error (make-dpt1 :switch :unknown))
  (signals type-error (make-dpt1 :unknown :on)))

(test create-dpt9-9.001
  (let ((dpt (make-dpt9 :temperature 23.5)))
    (is (= 2 (dpt-byte-len dpt)))
    (is (equalp #(1 46) (dpt-raw-value dpt)))
    (is (= (dpt-value dpt) 23.5))
    (is (equalp #(1 46) (to-byte-seq dpt))))
  (signals type-error (make-dpt9 :unknown 23.5))
  (signals type-error (make-dpt9 :temperature "23.5"))
  (signals type-error (make-dpt9 :temperature 23))
  )

(run! 'dpt-tests)
