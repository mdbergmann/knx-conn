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

;; ------------------------------------
;; dpt-1.001
;; ------------------------------------

(test create-dpt1-1.001
  ;; on
  (let ((dpt (make-dpt1 :switch :on)))
    (is (eq (dpt-value-type dpt) 'dpt-1.001))
    (is (eq (dpt-value dpt) :on))
    (is (equalp #(1) (knxobj:to-byte-seq dpt))))
  (let ((dpt (make-dpt1 'dpt-1.001 :on)))
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

(test toggle-dpt1-1.001
  (let* ((dpt (make-dpt1 :switch :on))
         (toggled (dpt1-toggle dpt)))
    (is (eq (dpt-value toggled) :off))))

;; ------------------------------------
;; dpt-9.001
;; ------------------------------------

(test create-dpt9-9.001
  (let ((dpt (make-dpt9 :temperature 23.5)))
    (is (eq (dpt-value-type dpt) 'dpt-9.001))
    (is (= 2 (dpt-byte-len dpt)))
    (is (= (dpt-value dpt) 23.5))
    (is (equalp #(12 151) (to-byte-seq dpt))))
  (signals type-error (make-dpt9 :unknown 23.5))
  (signals type-error (make-dpt9 :temperature "23.5"))
  (signals type-error (make-dpt9 :temperature 23))
  ;; enforce limits
  (signals dpt-out-of-bounds-error (make-dpt9 :temperature -274.0))
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

;; ------------------------------------
;; dpt-5.001
;; ------------------------------------

(test create-dpt5-5.001
  (let ((dpt (make-dpt5 :scaling 50)))
    (is (eq (dpt-value-type dpt) 'dpt-5.001))
    (is (= 1 (dpt-byte-len dpt)))
    (is (= (dpt-value dpt) 50))
    (is (<= 127 (aref (to-byte-seq dpt) 0) 129)))
  (let ((dpt (make-dpt5 :scaling 100)))
    (is (eq (dpt-value-type dpt) 'dpt-5.001))
    (is (= 1 (dpt-byte-len dpt)))
    (is (= (dpt-value dpt) 100))
    (is (= 255 (aref (to-byte-seq dpt) 0))))
  (signals type-error (make-dpt5 :unknown 23))
  (signals type-error (make-dpt5 :scaling "23.5"))
  (signals type-error (make-dpt5 :scaling 23.5))
  ;; enforce limits
  (signals type-error (make-dpt5 :scaling -24))
  (signals dpt-out-of-bounds-error (make-dpt5 :scaling 101)))

(test parse-dpt5-5.001
  (let ((dpt (parse-to-dpt
              (value-type-string-to-symbol "5.001")
              #(128))))
    (is (not (null dpt)))
    (is (= (dpt-value dpt) 50))
    (is (eq (dpt-value-type dpt) 'dpt-5.001)))
  (let ((dpt (parse-to-dpt
              (value-type-string-to-symbol "5.001")
              #(255))))
    (is (= (dpt-value dpt) 100))))

;; ------------------------------------
;; dpt-5.010
;; ------------------------------------

(test create-dpt5-5.010
  (let ((dpt (make-dpt5 :ucount 67)))
    (is (eq (dpt-value-type dpt) 'dpt-5.010))
    (is (= 1 (dpt-byte-len dpt)))
    (is (= (dpt-value dpt) 67))
    (is (equalp #(67) (to-byte-seq dpt))))
  (is-true (make-dpt5 'dpt-5.010 101))
  (signals type-error (make-dpt5 :unknown 23))
  (signals type-error (make-dpt5 :ucount "23.5"))
  (signals type-error (make-dpt5 :ucount 23.5))
  ;; enforce limits
  (signals type-error (make-dpt5 :ucount -24))
  (signals type-error (make-dpt5 :ucount 256)))

(test parse-dpt5-5.010
  (let ((dpt (parse-to-dpt
              (value-type-string-to-symbol "5.010")
              #(128))))
    (is (not (null dpt)))
    (is (= (dpt-value dpt) 128))
    (is (eq (dpt-value-type dpt) 'dpt-5.010)))
  (let ((dpt (parse-to-dpt
              (value-type-string-to-symbol "5.010")
              #(255))))
    (is (= (dpt-value dpt) 255))))

;; ------------------------------------
;; dpt-10.001
;; ------------------------------------

(test create-dpt10-10.001
  (let* ((now (local-time:now))
         (day (local-time:timestamp-day-of-week now))
         (hour (local-time:timestamp-hour now))
         (minute (local-time:timestamp-minute now))
         (second (local-time:timestamp-second now))
         (dpt (make-dpt10 now))
         (expected-oct3 (progn
                          (+ (ash (if (= day 0) 7 day) 5)
                             (logand hour #x1f)))))
    (is (eq (dpt-value-type dpt) 'dpt-10.001))
    (is (= 3 (dpt-byte-len dpt)))
    (is (eq now (dpt-value dpt)))
    (is (equalp (vector expected-oct3 minute second)
                (to-byte-seq dpt))))
  (signals type-error (make-dpt10 23)))

(test parse-dpt10-10.001
  (let ((dpt (parse-to-dpt
              (value-type-string-to-symbol "10.001")
              #(85 32 41))))
    (is (not (null dpt)))
    (let ((value (dpt-value dpt)))
      (is (typep value 'local-time:timestamp))
      (is (eq (dpt-value-type dpt) 'dpt-10.001))
      (is-true (and
                (= (local-time:timestamp-minute value) 32)
                (= (local-time:timestamp-second value) 41)
                (= (local-time:timestamp-day-of-week value) 2)
                (= (local-time:timestamp-hour value) 21))))))
