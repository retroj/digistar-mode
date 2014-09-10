
(require 'ert)

(require 'digistar-mode)

(ert-deftest digistar-mode-test-indentation ()
  "Tests digistar-indent-line-function."
  (with-temp-buffer
    (digistar-mode)
    (insert "+0.1\tfoo\n")
    (goto-char 0)
    (digistar-indent-line-function)
    (should (string= "+0.1\tfoo\n" (buffer-string))))
  (with-temp-buffer
    (digistar-mode)
    (insert "+0.1\t foo\n")
    (goto-char 0)
    (digistar-indent-line-function)
    (should (string= "+0.1    foo\n" (buffer-string)))))
