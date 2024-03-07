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
    (is (eq (dpt-value-type dpt) 'dpt-1.001))
    (is (eq (dpt-value dpt) :on))
    (is (equalp #(1) (knxobj:to-byte-seq dpt))))
  ;; off
  (let ((dpt (make-dpt1 :switch :off)))
    (is (eq (dpt-value dpt) :off))
    (is (equalp #(0) (to-byte-seq dpt))))
  ;; errors
  (signals type-error (make-dpt1 :switch :unknown))
  (signals type-error (make-dpt1 :unknown :on))
  ;; parse from byte-seq
  (let ((dpt (parse-to-dpt
              (value-type-string-to-symbol "1.001")
              #(1))))
    (is (not (null dpt)))
    (is (eq (dpt-value dpt) :on))
    (is (eq (dpt-value-type dpt) 'dpt-1.001))))

(test create-dpt9-9.001
  (let ((dpt (make-dpt9 :temperature 23.5)))
    (is (eq (dpt-value-type dpt) 'dpt-9.001))
    (is (= 2 (dpt-byte-len dpt)))
    (is (= (dpt-value dpt) 23.5))
    (is (equalp #(12 151) (to-byte-seq dpt))))
  (signals type-error (make-dpt9 :unknown 23.5))
  (signals type-error (make-dpt9 :temperature "23.5"))
  (signals type-error (make-dpt9 :temperature 23))
  ;; parse
  (let ((dpt (parse-to-dpt
              (value-type-string-to-symbol "9.001")
              #(12 151))))
    (is (not (null dpt)))
    (is (= (dpt-value dpt) 23.5))
    (is (eq (dpt-value-type dpt) 'dpt-9.001)))
  ;; 0
  (let ((dpt (make-dpt9 :temperature 0.0)))
    (is (= (dpt-value dpt) 0.0))
    (is (equalp #(0 0) (to-byte-seq dpt)))
    (is (equalp (parse-to-dpt
                 (value-type-string-to-symbol "9.001")
                 #(0 0))
                dpt)))
  ;; -123.5
  (let ((dpt (make-dpt9 :temperature -123.5)))
    (is (= (dpt-value dpt) -123.5))
    (is (equalp #(153 248) (to-byte-seq dpt)))
    (let ((parsed-dpt (parse-to-dpt
                       (value-type-string-to-symbol "9.001")
                       #(153 248))))
      (is (not (null parsed-dpt)))
      (is (= (dpt-value parsed-dpt) -123.52)))))

;; test with 0 and - values, boundaries, ...

