
(require 'digistar-mode)

(defun digistar-mrslog-hang-finder ()
  (interactive)

  (save-excursion
    (beginning-of-line)

    ;; go line by line
    (when (re-search-forward digistar-mrslog-line-re nil t)
      (let ((time-str (match-string 3))
            (ampm-str (match-string 4)))
        (goto-char (match-beginning 0))
        (when (digistar-mrslog-highlight-oid-line (point-at-eol))
          (let ((category (match-string 4))
                (cmd-timestamp-str (match-string 9))
                (cmd-command (match-string 10))
                (cmd-pathname (match-string 11)))

            (message "%s %s %s %s" time-str category cmd-timestamp-str cmd-command cmd-pathname)))
        
        (forward-line)))

    ;; if match digistar-mrslog-line-re
    ;; - group 1 is whole timestamp
    ;; - group 2 is date
    ;; - group 3 is time
    ;; - group 4 is AM/PM

    ;; if (digistar-mrslog-highlight-oid-line (point-at-eol)
    ;; - group 4 is category - we care about "CmdEcho"
    ;; - group 9 is cmdecho timestamp
    ;; - group 10 is cmdecho command
    ;; - group 11 is cmdecho command pathname


    ))
