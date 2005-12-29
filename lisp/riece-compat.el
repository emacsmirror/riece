;;; riece-compat.el --- compatibility functions
;; Copyright (C) 1998-2003 Daiki Ueno

;; Author: Daiki Ueno <ueno@unixuser.org>
;; Created: 1998-09-28
;; Keywords: IRC, riece, APEL

;; This file is part of Riece.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(if (featurep 'xemacs)
    (require 'riece-xemacs)
  (require 'riece-emacs))

(defalias 'riece-mode-line-buffer-identification
  'identity)

(defun riece-simplify-mode-line-format ()
  "Remove unnecessary information from `mode-line-format'."
  (let ((value (rassq 'mode-line-modified mode-line-format)))
    (if value
	(setq mode-line-format (delq value (copy-sequence mode-line-format)))
      mode-line-format)))

(defun riece-line-beginning-position ()
  (save-excursion
    (beginning-of-line)
    (point)))

(defun riece-line-end-position ()
  (save-excursion
    (end-of-line)
    (point)))

(autoload 'read-passwd "passwd")
(defvar riece-read-passwd #'read-passwd)
(defun riece-read-passwd (prompt)
  (funcall riece-read-passwd prompt))

(if (string-match "0\\{0\\}" "")
    (defun riece-make-interval-regexp (regexp min &optional max)
      (if max
	  (format "%s\\{%d,%d\\}" regexp min max)
	(format "%s\\{%d\\}" regexp min)))
  ;; Emacs 20.7 doesn't support \{...\} in regexps.
  (defun riece-make-interval-regexp (regexp min &optional max)
    (mapconcat #'identity
	       (nconc (make-list min regexp)
		      (if max
			  (make-list (- max min) (concat regexp "?"))))
	       "")))

(provide 'riece-compat)

;;; riece-compat.el ends here
