
(require 'buttercup)

(require 'digistar-mode)


(describe "digistar-timestamp-to-seconds"
  (it "\"0\" -> 0"
    (expect (digistar-timestamp-to-seconds "0") :to-equal 0))
  (it "\"1\" -> 1"
    (expect (digistar-timestamp-to-seconds "1") :to-equal 1))
  (it "\"1.5\" -> 1.5"
    (expect (digistar-timestamp-to-seconds "1.5") :to-equal 1.5))
  (it "\"1:01.5\" -> 61.5"
    (expect (digistar-timestamp-to-seconds "1:01.5") :to-equal 61.5))
  (it "\"1:00:01.5\" -> 3601.5"
    (expect (digistar-timestamp-to-seconds "1:00:01.5") :to-equal 3601.5)))


(defun digistar-test-absolute-time (setup)
  (let ((setup (split-string setup "|")))
    (with-temp-buffer
      (insert (cadr setup))
      (goto-char (point-min))
      (insert (car setup))
      (digistar-absolute-time-at-point))))

(describe "digistar-absolute-time-at-point"
  (it "\"3.0|\" -> 3.0"
    (expect (digistar-test-absolute-time "3.0|") :to-equal 3.0))
  (it "\"3.0\\n|\" -> 3.0"
    (expect (digistar-test-absolute-time "3.0\n|") :to-equal 3.0))
  (it "\"3.0\\n+3.0|\" -> 6.0"
    (expect (digistar-test-absolute-time "3.0\n+3.0|") :to-equal 6.0))
  (it "\"\\n+3.0|\" -> 3.0"
    (expect (digistar-test-absolute-time "\n+3.0|") :to-equal 3.0)))


(describe "digistar-format-decimal-number"
  (it "0.999999 -> \"1\""
    (expect (digistar-format-decimal-number 0.999999) :to-equal "1"))
  (it "0.9999998 -> \"1\""
    (expect (digistar-format-decimal-number 0.9999998) :to-equal "1"))
  (it "0.89999998 -> \"0.9\""
    (expect (digistar-format-decimal-number 0.89999998) :to-equal "0.9"))
  (it "0.8899999998 -> \"0.89\""
    (expect (digistar-format-decimal-number 0.8899999998) :to-equal "0.89"))
  (it "0.328 -> \"0.328\""
    (expect (digistar-format-decimal-number 0.328) :to-equal "0.328"))
  (it "0.00000001 -> \"0\""
    (expect (digistar-format-decimal-number 0.00000001) :to-equal "0"))
  (it "4.211 -> \"4.211\""
    (expect (digistar-format-decimal-number 4.211) :to-equal "4.211")))


(defun digistar-test-tab-command (line pt)
  (let ((this-command 'blah)) ;; workaround because this isn't interactive
    (with-temp-buffer
      (insert line)
      (goto-char pt)
      (digistar-mode)
      (digistar-indent-for-tab-command)
      (list (buffer-string) (point)))))

(describe "digistar-indent-for-tab-command"
  (it "timestamp, no command, cursor point-min"
      (expect (digistar-test-tab-command "1" 1)
              :to-equal '("1" 1)))

  (it "indented timestamp, no command, cursor point-min"
      (expect (digistar-test-tab-command " 1" 1)
              :to-equal '("1" 1)))

  (it "timestamp tab command, cursor point-min"
      (expect (digistar-test-tab-command "1\tsystem skyon" 1)
              :to-equal '("1\tsystem skyon" 1)))

  (it "indented timestamp tab command, cursor point-min"
      (expect (digistar-test-tab-command " 1\tsystem skyon" 1)
              :to-equal '("1\tsystem skyon" 1)))

  (it "indented timestamp space command, cursor point-min"
      (expect (digistar-test-tab-command " 1 system skyon" 1)
              :to-equal '("1\tsystem skyon" 1)))

  (it "no timestamp + command, cursor point-min"
      (expect (digistar-test-tab-command "system skyon" 1)
              :to-equal '("\tsystem skyon" 2)))

  (it "timestamp, no command, cursor position 2"
      (expect (digistar-test-tab-command "1" 2)
              :to-equal '("1\t" 3)))

  (it "timestamp, space, command, cursor position 2"
      (expect (digistar-test-tab-command "1 system skyon" 2)
              :to-equal '("1\tsystem skyon" 3)))

  (it "timestamp, space, command, cursor position 3"
      (expect (digistar-test-tab-command "1 system skyon" 3)
              :to-equal '("1\tsystem skyon" 3)))

  (it "timestamp, space, command, cursor position 4"
      (expect (digistar-test-tab-command "1 system skyon" 4)
              :to-equal '("1\tsystem skyon" 4)))

  (it "timestamp, morespace, command, cursor position 3"
      (expect (digistar-test-tab-command "1   system skyon" 3)
              :to-equal '("1\tsystem skyon" 3)))

  (it "timestamp, morespace, command, cursor position 4"
      (expect (digistar-test-tab-command "1   system skyon" 4)
              :to-equal '("1\tsystem skyon" 3)))

  (it "timestamp, morespace, command, cursor position 5"
      (expect (digistar-test-tab-command "1   system skyon" 5)
              :to-equal '("1\tsystem skyon" 3)))

  (it "timestamp, morespace, command, cursor position 7"
      (expect (digistar-test-tab-command "1   system skyon" 7)
              :to-equal '("1\tsystem skyon" 5)))

  (it "timestamp, tab, command, cursor position 2"
      (expect (digistar-test-tab-command "1\tsystem skyon" 2)
              :to-equal '("1\tsystem skyon" 3)))

  )

