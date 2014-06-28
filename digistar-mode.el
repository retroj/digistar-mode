;;; digistar-mode.el --- major mode for Digistar scripts

;; Copyright (C) 2014  John Foerch <jjfoerch@earthlink.net>

;; Author: John Foerch <jjfoerch@earthlink.net>
;; Version: 0.1
;; Date: 2014-06-27
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
;; scripts.

;;; Code:

(defvar digistar-indent 8)

(defvar digistar-font-lock-keywords
  `(;; digistar version cookie
    "^# {\\[[0-9.]+]}"))

(defun digistar-indent-line-function ()
  (let (timestamp
        command)
    (save-excursion
      (beginning-of-line)
      (when (looking-at
             "[[:blank:]]*\\(\\+?[0-9.]+\\)?[[:blank:]]*\\(.+\\)?$")
        (setq timestamp (match-string 1))
        (setq command (match-string 2))))
    (cond
     ((and timestamp command)
      (indent-line-to 0)
      (save-excursion
        (re-search-forward "\\s-")
        (re-search-forward "\\sw")
        (backward-char)
        (if (< (current-column) digistar-indent)
            (indent-to digistar-indent)
          (delete-region (+ (point-at-bol) digistar-indent)
                         (point)))))
     (timestamp
      (indent-line-to 0))
     (command
      (indent-line-to digistar-indent))
     ((= (current-column) digistar-indent)
      (delete-region (point-at-bol) (point)))
     (t
      (delete-region (point-at-bol) (point))
      (insert (make-string digistar-indent 32))))))

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
  (set (make-local-variable 'font-lock-defaults)
       (list digistar-font-lock-keywords))
  (set (make-local-variable 'show-trailing-whitespace) t)

  ;; Comments
  (set (make-local-variable 'comment-start) "# ")
  (set (make-local-variable 'comment-end) "")

  ;; Whitespace
  (set (make-local-variable 'indent-tabs-mode) nil)
  (set (make-local-variable 'require-final-newline) t))

(provide 'digistar-mode)
;;; digistar-mode.el ends here
