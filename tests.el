;;; tests.el --- tests for i-ching.el -*- coding: utf-8; lexical-binding: t -*-

;;; Commentary:
;;
;; some simple tests...

;;; Code:

(require 'ert)
(require 'with-simulated-input)
(require 'i-ching)

;; range 0-63 NOTE: doesn't fail when rate-limit is hit
(ert-deftest i-ching-qrnd ()
             (should (numberp (i-ching-q64)))
             (should (<= 0 (i-ching-q64)))
             (should (> 64 (i-ching-q64))))

;; range 0-63
(ert-deftest i-ching-rrnd ()
             (should (numberp (i-ching-r64)))
             (should (<= 0 (i-ching-r64)))
             (should (> 64 (i-ching-r64))))

(ert-deftest i-ching-random ()
             (should (numberp (i-ching-random 64 'quantum)))
             (should (numberp (i-ching-random 64 'atmospheric)))
             (should (numberp (i-ching-random 64 'pseudo)))
             (should (numberp (i-ching-random 64))))

(ert-deftest i-ching-n2h ()
             (should (string= "䷁" (i-ching-number-to-hexagram 2)))
             (should (string= "䷩" (i-ching-number-to-hexagram 42)))
             (should (string= nil (i-ching-number-to-hexagram 65))))

(ert-deftest i-ching-h2n ()
             (should (eql 2 (i-ching-hexagram-to-number "䷁")))
             (should (eql 40 (i-ching-hexagram-to-number "䷧")))
             (should-not (i-ching-hexagram-to-number "NOT")))

(ert-deftest i-ching-b2h ()
             (string= "䷧" (i-ching-binary-to-hexagram #b010100)))


(ert-deftest i-ching-names ()
             (should-not (i-ching-number-to-name 0))
             (should-not (i-ching-number-to-name 65))
             (should (string= "HEXAGRAM FOR BEFORE COMPLETION"
                              (i-ching-number-to-unicode-name 64)))
             (should (string= "Before Completion"
                              (i-ching-number-to-name 64))))

(ert-deftest i-ching-descriptions ()
             (should (stringp (i-ching-number-to-description 43)))
             (should (stringp (i-ching-number-to-judgment 27)))
             (should (stringp (i-ching-number-to-image 13)))
             (should-not (stringp (i-ching-number-to-description 0)))
             (should-not (stringp (i-ching-number-to-judgment 0)))
             (should-not (stringp (i-ching-number-to-image 0)))
             (should-not (stringp (i-ching-number-to-description 65)))
             (should-not (stringp (i-ching-number-to-judgment 65)))
             (should-not (stringp (i-ching-number-to-image 65))))

(ert-deftest i-ching-all-descriptions ()
             (dotimes (i 63)
               (should-not (seq-empty-p (i-ching-number-to-unicode-name (1+ i))))
               (should-not (seq-empty-p (i-ching-number-to-description (1+ i))))
               (should-not (seq-empty-p (i-ching-number-to-judgment (1+ i))))
               (should-not (seq-empty-p (i-ching-number-to-image (1+ i))))
               (should-not (seq-empty-p (i-ching-number-to-name (1+ i))))
               (should (stringp (i-ching-number-to-unicode-name (1+ i))))
               (should (stringp (i-ching-number-to-description (1+ i))))
               (should (stringp (i-ching-number-to-judgment (1+ i))))
               (should (stringp (i-ching-number-to-image (1+ i))))
               (should (stringp (i-ching-number-to-name (1+ i))))))

(ert-deftest i-ching-interprets ()
             (should (stringp (i-ching-interpretation 49)))
             (should-not (seq-empty-p  (i-ching-interpretation 49))))

(ert-deftest i-ching-queries ()
             (should (stringp (i-ching-query-string)))
             (should (stringp (i-ching-query-string '3-coins))))

(ert-deftest i-ching-castings ()
             (should (stringp (i-ching-cast 'yarrow-stalks)))
             (should (stringp (i-ching-cast '3-coins)))
             (should (stringp (i-ching-cast))))


;; randomness

(defun χ² (expected observed)
  "Chi-squared test for EXPECTED OBSERVED sequences."
  (cl-reduce #'+
             (cl-mapcar
              (lambda (e o) (/ (expt (- o e) 2) e))
              expected observed)))

;; (mapcar (lambda (n)
;;           (i-ching-random n 'atmospheric))
;;         (make-list 1024 64))


;; interactive testing

;; (i-ching-query '6-bit)
;; (i-ching-query '3-coins)
;; (i-ching-query 'yarrow-stalks)

(ert-deftest i-ching-query-6bit ()
             (should (with-simulated-input
                       "testing SPC circumstances RET"
                       (i-ching-query '6-bit))))

(ert-deftest i-ching-query-3coin ()
             (should (with-simulated-input
                       "testing SPC circumstances RET"
                       (i-ching-query '3-coins))))

(ert-deftest i-ching-query-yarrow ()
             (should (with-simulated-input
                       "testing SPC circumstances RET"
                       (i-ching-query 'yarrow-stalks))))

;; run them...

(ert t)

;;; tests.el ends here
