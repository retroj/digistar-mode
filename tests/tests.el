
(require 'ert)

(require 'digistar-mode)

(defun digistar-test-indentation-case (setup)
  (let ((setup (split-string setup "|")))
    (with-temp-buffer
      (insert (cadr setup))
      (goto-char (point-min))
      (insert (car setup))
      (digistar-indent-line-function)
      (concat (buffer-substring (point-min) (point)) "|"
              (buffer-substring (point) (point-max))))))

(ert-deftest digistar-test-indent-blank-01 ()
  (should (string= "        |" (digistar-test-indentation-case "|"))))

(ert-deftest digistar-test-indent-blank-02 ()
  (should (string= "        |" (digistar-test-indentation-case "|  "))))

(ert-deftest digistar-test-indent-blank-03 ()
  (should (string= "|" (digistar-test-indentation-case "        |"))))

(ert-deftest digistar-test-indent-blank-04 ()
  (should (string= "|" (digistar-test-indentation-case "\t|"))))

(ert-deftest digistar-test-indent-blank-05 ()
  (should (string= "|" (digistar-test-indentation-case "        | "))))

(ert-deftest digistar-test-indent-blank-06 ()
  (should (string= "        |" (digistar-test-indentation-case "   |"))))

(ert-deftest digistar-test-indent-blank-07 ()
  (should (string= "        |" (digistar-test-indentation-case "   | "))))

(ert-deftest digistar-test-indent-blank-08 ()
  (should (string= "        |" (digistar-test-indentation-case "          |"))))

(ert-deftest digistar-test-indent-blank-09 ()
  (should (string= "        |" (digistar-test-indentation-case "          |  "))))

(ert-deftest digistar-test-indent-blank-10 ()
  (should (string= "        |" (digistar-test-indentation-case "\t  |"))))

(ert-deftest digistar-test-indent-comment-01 ()
  (should (string= "        # foo|" (digistar-test-indentation-case "# foo|"))))

(ert-deftest digistar-test-indent-comment-02 ()
  (should (string= "# foo|" (digistar-test-indentation-case "        # foo|"))))

(ert-deftest digistar-test-indent-comment-03 ()
  (should (string= "        # f|oo" (digistar-test-indentation-case "   # f|oo"))))

(ert-deftest digistar-test-indent-comment-04 ()
  (should (string= "        |# foo" (digistar-test-indentation-case "|# foo"))))

(ert-deftest digistar-test-indent-comment-05 ()
  (should (string= "        |# foo" (digistar-test-indentation-case " | # foo"))))

(ert-deftest digistar-test-indent-timestamp-01 ()
  (should (string= "|0" (digistar-test-indentation-case "|0"))))

(ert-deftest digistar-test-indent-timestamp-02 ()
  (should (string= "1|0" (digistar-test-indentation-case "1|0"))))

(ert-deftest digistar-test-indent-timestamp-03 ()
  (should (string= "10      |" (digistar-test-indentation-case "10|"))))

(ert-deftest digistar-test-indent-timestamp-04 ()
  (should (string= "10|" (digistar-test-indentation-case "10      |"))))

(ert-deftest digistar-test-indent-timestamp-05 ()
  (should (string= "10      |" (digistar-test-indentation-case "10  |"))))

(ert-deftest digistar-test-indent-timestamp-06 ()
  (should (string= "10      |" (digistar-test-indentation-case "10        |"))))

(ert-deftest digistar-test-indent-timestamp-07 ()
  (should (string= "|10" (digistar-test-indentation-case "| 10"))))

(ert-deftest digistar-test-indent-timestamp-08 ()
  (should (string= "|10" (digistar-test-indentation-case " |10"))))

(ert-deftest digistar-test-indent-timestamp-09 ()
  (should (string= "1|0" (digistar-test-indentation-case " 1|0"))))

(ert-deftest digistar-test-indent-timestamp-10 ()
  (should (string= "10      |" (digistar-test-indentation-case " 10|"))))

(ert-deftest digistar-test-indent-timestamp-11 ()
  (should (string= "10      |" (digistar-test-indentation-case " 10 |"))))

(ert-deftest digistar-test-indent-timestamp-12 ()
  (should (string= "+1:00   |" (digistar-test-indentation-case "+1:00|"))))

(ert-deftest digistar-test-indent-command-01 ()
  (should (string= "        |foo" (digistar-test-indentation-case "|foo"))))

(ert-deftest digistar-test-indent-command-02 ()
  (should (string= "        f|oo" (digistar-test-indentation-case "f|oo"))))

(ert-deftest digistar-test-indent-command-03 ()
  (should (string= "        |foo" (digistar-test-indentation-case " |foo"))))

(ert-deftest digistar-test-indent-command-04 ()
  (should (string= "        |foo" (digistar-test-indentation-case " | foo"))))

(ert-deftest digistar-test-indent-timestamp-command-01 ()
 (should (string= "|10      foo" (digistar-test-indentation-case "|10  foo"))))

(ert-deftest digistar-test-indent-timestamp-command-02 ()
  (should (string= "1|0      foo" (digistar-test-indentation-case "1|0  foo"))))

(ert-deftest digistar-test-indent-timestamp-command-03 ()
  (should (string= "10      |foo" (digistar-test-indentation-case "10|  foo"))))

(ert-deftest digistar-test-indent-timestamp-command-04 ()
  (should (string= "10      |foo" (digistar-test-indentation-case "10 | foo"))))

(ert-deftest digistar-test-indent-timestamp-command-05 ()
  (should (string= "10      |foo" (digistar-test-indentation-case "10  |foo"))))

(ert-deftest digistar-test-indent-timestamp-command-06 ()
  (should (string= "10      f|oo" (digistar-test-indentation-case "10  f|oo"))))

(ert-deftest digistar-test-indent-timestamp-command-07 ()
  (should (string= "+0.1    |foo" (digistar-test-indentation-case "+0.1|\tfoo"))))

(ert-deftest digistar-test-indent-timestamp-command-08 ()
  (should (string= "+0.1    |foo" (digistar-test-indentation-case "+0.1|\t foo"))))

(ert-deftest digistar-test-indent-timestamp-command-09 ()
  (should (string= "10      |foo" (digistar-test-indentation-case " 10|  foo"))))
