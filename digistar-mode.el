;;; digistar-mode.el --- major mode for Digistar script format

;; Copyright (C) 2014  John Foerch <jjfoerch@earthlink.net>

;; Author: John Foerch <jjfoerch@earthlink.net>
;; Version: 0.1
;; Date: 2014-06-26
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

;;; Code:

(defvar digistar--font-lock-keywords
  `("^# {\\[[0-9.]+]}"))

(define-derived-mode digistar-mode prog-mode
  "Digistar"
  "A major mode for Digistar scripts.
\\{digistar-mode-map}"

  ;; Syntax Highlighting
  (set (make-local-variable 'font-lock-defaults)
       (list digistar--font-lock-keywords))
  (set (make-local-variable 'show-trailing-whitespace) t)

  ;; Comments
  (set (make-local-variable 'comment-start) "# ")
  (set (make-local-variable 'comment-end) ""))

(provide 'digistar-mode)
;;; digistar-mode.el ends here
