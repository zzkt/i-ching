;;; tests.el --- tests for i-ching.el -*- coding: utf-8; lexical-binding: t -*-

;; some simple tests...

(require 'i-ching)

(ert-deftest qrnd ()
             (should (numberp (i-ching-q64)))
             (should (< 0 (i-ching-q64)))
             (should (>= 64 (i-ching-q64))))

(ert-deftest rrnd ()
             (should (numberp (i-ching-r64)))
             (should (< 0 (i-ching-r64)))
             (should (>= 64 (i-ching-r64))))

(ert-deftest i-ching-n2h ()
             (should (string= "䷁" (i-ching-number-to-hexagram 2)))
             (should (string= "䷩" (i-ching-number-to-hexagram 42)))
             (should (string= nil (i-ching-number-to-hexagram 65))))

(ert-deftest i-ching-h2n ()
             (should (eql 2 (i-ching-hexagram-to-number "䷁")))
             (should (eql 40 (i-ching-hexagram-to-number "䷧")))
             (should (eql nil (i-ching-hexagram-to-number "NOT"))))

(ert-deftest i-ching-b2h ()
             (string= "䷧" (i-ching-binary-to-hexagram #b010100)))

(ert-deftest i-ching-name ()
             (string= "HEXAGRAM FOR BEFORE COMPLETION"  (i-ching-number-to-unicode-name 64)))

(ert-deftest i-ching-castings ()
             (should (stringp (i-ching-cast 'yarrow-stalks)))
             (should (stringp(i-ching-cast '3-coins)))
             (should (stringp(i-ching-cast))))

(ert t)

;;; tests.el ends here
