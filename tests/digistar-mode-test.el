
(require 'buttercup)
(require 'faceup)

(require 'digistar-mode)


;;
;; Timestamps
;;

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


;;
;; Indentation
;;

(defun digistar-test-tab-command (line pt)
  (let ((this-command 'blah)) ;; workaround because this isn't interactive
    (with-temp-buffer
      (insert line)
      (goto-char pt)
      (digistar-mode)
      (digistar-indent-for-tab-command)
      (list (buffer-string) (point)))))

(describe "digistar-indent-for-tab-command"
  (it "'|1'                -> '|1'"
      (expect (digistar-test-tab-command "1" 1)
              :to-equal '("1" 1)))

  (it "'| 1'               -> '|1'"
      (expect (digistar-test-tab-command " 1" 1)
              :to-equal '("1" 1)))

  (it "'|1\\tsystem skyon'  -> '|1\\tsystem skyon'"
      (expect (digistar-test-tab-command "1\tsystem skyon" 1)
              :to-equal '("1\tsystem skyon" 1)))

  (it "'| 1\\tsystem skyon' -> '|1\\tsystem skyon'"
      (expect (digistar-test-tab-command " 1\tsystem skyon" 1)
              :to-equal '("1\tsystem skyon" 1)))

  (it "'| 1 system skyon'  -> '|1\\tsystem skyon'"
      (expect (digistar-test-tab-command " 1 system skyon" 1)
              :to-equal '("1\tsystem skyon" 1)))

  (it "'|system skyon'     -> '\\t|system skyon'"
      (expect (digistar-test-tab-command "system skyon" 1)
              :to-equal '("\tsystem skyon" 2)))

  (it "'1|'                -> '1\\t|'"
      (expect (digistar-test-tab-command "1" 2)
              :to-equal '("1\t" 3)))

  (it "'1| system skyon'   -> '1\\t|system skyon'"
      (expect (digistar-test-tab-command "1 system skyon" 2)
              :to-equal '("1\tsystem skyon" 3)))

  (it "'1 |system skyon'   -> '1\\t|system skyon'"
      (expect (digistar-test-tab-command "1 system skyon" 3)
              :to-equal '("1\tsystem skyon" 3)))

  (it "'1 s|ystem skyon'   -> '1\\ts|ystem skyon'"
      (expect (digistar-test-tab-command "1 system skyon" 4)
              :to-equal '("1\tsystem skyon" 4)))

  (it "'1 |  system skyon' -> '1\\t|system skyon'"
      (expect (digistar-test-tab-command "1   system skyon" 3)
              :to-equal '("1\tsystem skyon" 3)))

  (it "'1  | system skyon' -> '1\\t|system skyon'"
      (expect (digistar-test-tab-command "1   system skyon" 4)
              :to-equal '("1\tsystem skyon" 3)))

  (it "'1   |system skyon' -> '1\\t|system skyon'"
      (expect (digistar-test-tab-command "1   system skyon" 5)
              :to-equal '("1\tsystem skyon" 3)))

  (it "'1   sy|stem skyon' -> '1\\tsy|stem skyon'"
      (expect (digistar-test-tab-command "1   system skyon" 7)
              :to-equal '("1\tsystem skyon" 5)))

  (it "'1|\\tsystem skyon'  -> '1\\t|system skyon'"
      (expect (digistar-test-tab-command "1\tsystem skyon" 2)
              :to-equal '("1\tsystem skyon" 3)))

  )


;;
;; Syntax highlighting (font-lock)
;;

 ;; faceup cheat sheet:
 ;; (font-lock-builtin-face . "b")
 ;; (font-lock-comment-delimiter-face . "m")
 ;; (font-lock-comment-face . "x")
 ;; (font-lock-constant-face . "c")
 ;; (font-lock-doc-face . "d")
 ;; (font-lock-function-name-face . "f")
 ;; (font-lock-keyword-face . "k")
 ;; (font-lock-negation-char-face . "n")
 ;; (font-lock-preprocessor-face . "p")
 ;; (font-lock-regexp-grouping-backslash . "h")
 ;; (font-lock-regexp-grouping-construct . "o")
 ;; (font-lock-string-face . "s")
 ;; (font-lock-type-face . "t")
 ;; (font-lock-variable-name-face . "v")
 ;; (font-lock-warning-face . "w"))


(defun digistar-font-lock-test (faceup)
  (faceup-test-font-lock-string 'digistar-mode faceup))
(faceup-defexplainer digistar-font-lock-test)

(describe "Syntax highlighting"
  (it "'«p:3»'                             "
    (expect (digistar-font-lock-test "«p:3»")))
  (it "'«p:+3»'                            "
    (expect (digistar-font-lock-test "«p:+3»")))
  (it "'«p:3»\\tfoo bar'                    "
    (expect (digistar-font-lock-test "«p:3»\tfoo bar")))
  (it "'foo «k:is»'                        "
    (expect (digistar-font-lock-test "foo «k:is»")))
  (it "'foo «k:is» «t:cameraClass»'        "
    (expect (digistar-font-lock-test "foo «k:is» «t:cameraClass»")))
  (it "'foo «k:add» bar'                   "
    (expect (digistar-font-lock-test "foo «k:add» bar")))
  (it "'\\t«# comments»'                    "
    (expect (digistar-font-lock-test "	«x:# comments»")))
  (it "'\\tfoo bar baz «# comments»'        "
    (expect (digistar-font-lock-test "	foo bar baz «x:# comments»")))
  (it "'foo «k:delete»'                    "
    (expect (digistar-font-lock-test "foo «k:delete»")))
  (it "'foo «k:on»'                        "
    (expect (digistar-font-lock-test "foo «k:on»")))
  (it "'foo «k:off»'                       "
    (expect (digistar-font-lock-test "foo «k:off»")))
  (it "'foo «k:moveto» bar'                "
    (expect (digistar-font-lock-test "foo «k:moveto» bar")))
  (it "'foo «k:turnto» bar'                "
    (expect (digistar-font-lock-test "foo «k:turnto» bar")))
  (it "'foo bar baz «duration» «3»'        "
    (expect (digistar-font-lock-test "foo bar baz «k:duration» «c:3»")))
  (it "'foo bar baz «duration» «3» 2 1'    "
    (expect (digistar-font-lock-test "foo bar baz «k:duration» «c:3» 2 1")))
  (it "'foo bar baz «dur» «3»'             "
    (expect (digistar-font-lock-test "foo bar baz «k:dur» «c:3»")))
  (it "'foo model «s:$Content/Library/Models/Misc/image.x»'"
      (expect (digistar-font-lock-test "foo model «s:$Content/Library/Models/Misc/image.x»")))
  (it "'«k:script» play «s:effects/fakemoon/fakemoon-is fklj.ds»'"
      (expect (digistar-font-lock-test "«k:script» play «s:effects/fakemoon/fakemoon-is fklj.ds»")))

  )
