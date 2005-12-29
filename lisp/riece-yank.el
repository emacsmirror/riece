;;; riece-yank.el --- enter the element of kill-ring
;; Copyright (C) 2004 Masatake YAMATO

;; Author: Masatake YAMATO <jet@gyve.org>
;; Keywords: IRC, riece

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

;;; Commentary:

;; NOTE: This is an add-on module for Riece.

;;; Code:
(require 'riece-commands)

(defgroup riece-yank nil
  "Enter the element of kill-ring."
  :tag "Yank"
  :prefix "riece-"
  :group 'riece)

(defcustom riece-yank-tick 1
  "Time span in second to send multiple lines."
  :type 'number
  :group 'riece-yank)

(defcustom riece-yank-strip-space nil
  "If non-nil, strip common spaces in front of lines and blank lines
before/after the first/last non-blank line."
  :type 'boolean
  :group 'riece-yank)

(defconst riece-yank-description
  "Enter the element of kill-ring.")

(defun riece-yank-insinuate ()
  )

(defvar riece-command-mode-map)
(defun riece-yank-enable ()
  (define-key riece-command-mode-map "\C-cy" 'riece-command-yank))
(defun riece-yank-disable ()
  (define-key riece-command-mode-map "\C-cy" 'undefined))

(defun riece-yank-strip-space (string)
  (with-temp-buffer
    (insert string)
    (untabify (point-min) (point-max))
    ;; Delete blank lines before the first non-blank line.
    (goto-char (point-min))
    (while (looking-at " *$")
      (delete-region (point) (progn (forward-line) (point))))
    ;; Delete blank lines after the last non-blank line.
    (goto-char (point-max))
    (while (progn (beginning-of-line) (looking-at " *$"))
      (delete-region (point) (progn (end-of-line 0) (point))))
    ;; Delete common spaces in front of lines.
    (let ((space-width (point-max)))
      (while (looking-at " +")
	(setq space-width (min space-width (length (match-string 0))))
	(forward-line))
      (goto-char (point-min))
      (while (not (eobp))
	(delete-char space-width)
	(forward-line)))
    (buffer-string)))

(defun riece-command-yank (arg prefix)
  (interactive "P\nsPrefix: ")
  (when (or (not prefix)
	    (string= prefix ""))
    (setq prefix " "))
  (let* ((kill (current-kill 0))
	 msg)
    (unless kill
      (error "Nothing to send in kill-ring"))
    (if riece-yank-strip-space
	(setq kill (riece-yank-strip-space kill)))
    (setq msg (split-string kill "\n"))
    (when (y-or-n-p (format "Send \"%s\"\n? " kill))
      (mapcar
       (lambda (x) 
	 (riece-command-send-message (concat prefix x) arg)
	 ;; Without next line, you will be kicked out from ircd.
	 ;; It may mean "Don't send much data at once."
	 (sit-for riece-yank-tick))
       msg))))

(provide 'riece-yank)
;;; riece-yank.el ends here
