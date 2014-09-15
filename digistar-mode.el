;;; digistar-mode.el --- major mode for Digistar scripts

;; Copyright (C) 2014  John Foerch <jjfoerch@earthlink.net>

;; Author: John Foerch <jjfoerch@earthlink.net>
;; Version: 0.3
;; Date: 2014-09-13
;; Keywords: languages

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides digistar-mode, a major mode for editing Digistar
;; scripts.  If installed via elpa, the auto-mode-list entry for this mode
;; will be setup automatically.  If installed manually, use a snippet like
;; the following to set it up:
;;
;;     (when (locate-library "digistar-mode")
;;      (add-to-list 'auto-mode-alist '("\\.ds\\'" . digistar-mode)))
;;

;;; Code:

(defvar digistar-indent 8
  "Indentation column for commands in a Digistar script.")

(defvar digistar-font-lock-keywords
  `(;; digistar version cookie
    "^# {\\[[0-9.]+]}"

    ;; timestamps
    ("^[[:blank:]]*\\(\\+?[0-9:.]+\\)"
     (1 font-lock-preprocessor-face))))

(defun digistar-indent-line-function ()
  "An indent-line-function for Digistar scripts.  Indents
timestamps to column 0 and commands to the value of
`digistar-indent'."
  (let ((col (current-column))
        (eol (point-at-eol))
        bol
        line-is-blank
        line-is-comment
        comment-start
        comment-column
        timestamp-start
        timestamp-end
        command-start)
    (save-excursion
      (beginning-of-line)
      (setq bol (point))
      (cond
       ((looking-at "[[:blank:]]*$")
        (setq line-is-blank t))
       ((looking-at "[[:blank:]]*\\(#\\)")
        (setq line-is-comment t)
        (setq comment-start (match-beginning 1))
        (goto-char comment-start)
        (setq comment-column (current-column)))
       ((looking-at "[[:blank:]]*\\(\\+?[0-9:.]+\\)?[[:blank:]]*\\(.+\\)?$")
        (setq timestamp-start (match-beginning 1)
              timestamp-end (match-end 1)
              command-start (match-beginning 2)))))
    (cond
     (line-is-blank
      (if (= digistar-indent col)
          (delete-region bol eol)
        (delete-region bol eol)
        (insert (make-string digistar-indent 32))))
     (line-is-comment
      (delete-region bol comment-start)
      (unless (= digistar-indent comment-column)
        (if (= (point) bol)
            (insert (make-string digistar-indent 32))
          (save-excursion
            (goto-char bol)
            (insert (make-string digistar-indent 32))))))
     ((and timestamp-start command-start)
      (delete-region timestamp-end command-start)
      (if (= (point) timestamp-end)
          (insert (make-string (- digistar-indent (- timestamp-end timestamp-start)) 32))
        (save-excursion
          (goto-char timestamp-end)
          (insert (make-string (- digistar-indent (- timestamp-end timestamp-start)) 32))))
      (delete-region bol timestamp-start))
     (timestamp-start
      (cond
       ((> timestamp-start bol)
        (let ((indent (>= (point) timestamp-end)))
          (delete-region bol timestamp-start)
          (when indent
            (insert (make-string (- digistar-indent (- (point) bol)) 32)))))
       ((= digistar-indent col)
        (delete-region timestamp-end eol))
       ((and (= bol timestamp-start)
             (>= col (- timestamp-end bol)))
        (delete-region timestamp-end eol)
        (insert (make-string (- digistar-indent (- timestamp-end timestamp-start)) 32)))))
     (command-start
      (delete-region bol command-start)
      (if (= (point) bol)
          (insert (make-string digistar-indent 32))
        (save-excursion
          (goto-char bol)
          (insert (make-string digistar-indent 32))))))))

(defalias 'digistar-parent-mode
  (if (fboundp 'prog-mode) 'prog-mode 'fundamental-mode))

;;;###autoload
(define-derived-mode digistar-mode digistar-parent-mode
  "Digistar"
  "A major mode for Digistar scripts.

\\{digistar-mode-map}"

  ;; Indentation
  (set (make-local-variable 'indent-line-function)
       'digistar-indent-line-function)

  ;; Syntax Highlighting
  (setq font-lock-defaults (list digistar-font-lock-keywords))
  (set (make-local-variable 'show-trailing-whitespace) t)

  ;; Comments
  (set (make-local-variable 'comment-start) "# ")
  (set (make-local-variable 'comment-end) "")

  ;; Whitespace
  (set (make-local-variable 'indent-tabs-mode) nil)
  (set (make-local-variable 'require-final-newline) t))


;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ds\\'" . digistar-mode))


(provide 'digistar-mode)
;;; digistar-mode.el ends here
