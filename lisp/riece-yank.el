;;; riece-kill.el --- enter the element in kill-ring
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
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; In riece's command buffer, you can send the top element of kill-ring
;; by C-c y. 
;; Don't forget do (riece-command-enable-addon 'riece-yank).
;;
;;; Code:
(require 'riece-commands)

(defgroup riece-yank nil
  "Enter the element of `kill-ring'"
  :tag "Yank"
  :prefix "riece-"
  :group 'riece)

(defcustom riece-yank-tick 1
  "Time span in second to send multiple lines."
  :type 'number
  :group 'riece-yank)

(defvar riece-yank-enabled nil)

(defun riece-yank-insinuate ()
  )

(defun riece-yank-enable ()
  (define-key riece-command-mode-map "\C-cy" 'riece-command-yank)
  (setq riece-yank-enabled t))
(defun riece-yank-disable ()
  (define-key riece-command-mode-map "\C-cy" 'undefined)
  (setq riece-yank-enabled nil))

(defun riece-command-yank (prefix)
  (interactive "sPrefix: ")
  (when (or (not prefix)
	    (string= prefix ""))
    (setq prefix " "))
  (let* ((kill (current-kill 0))
	 msg)
    (unless kill
      (error "Nothing to send in kill-ring"))
    (setq msg (split-string kill "\n"))
    (when (y-or-n-p (format "Send \"%s\"\n? " kill))
      (mapcar
       (lambda (x) 
	 (riece-command-send-message (concat prefix x) nil)
	 ;; Without next line, you will be kicked out from ircd.
	 ;; It may means "Don't send much data at once."
	 (sit-for riece-yank-tick))
       msg))))

(provide 'riece-yank)
;;; riece-yank.el ends here
