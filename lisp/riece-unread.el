;;; riece-unread.el --- "unread message mark" add-on
;; Copyright (C) 1998-2003 Daiki Ueno

;; Author: Daiki Ueno <ueno@unixuser.org>
;; Keywords: IRC, riece

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
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This add-on displays unread mark ("!") for channels which have
;; "unread messages".

;; To use, add the following line to your ~/.riece/init.el:
;; (add-to-list 'riece-addons 'riece-unread t)

;;; Code:

(eval-when-compile (require 'riece-message))

(defvar riece-unread-channels nil)

(defun riece-unread-display-message-function (message)
  (unless (or (riece-message-own-p message)
	      (and (equal (riece-message-target message) riece-current-channel)
		   (eq (window-buffer (selected-window))
		       (get-buffer riece-command-buffer))))
    (setq riece-unread-channels
	  (delete (riece-message-target message) riece-unread-channels))
    (add-to-list 'riece-unread-channels
		 (riece-message-target message))
    (riece-unread-update-channel-list-buffer)))

(defun riece-unread-channel-switch-hook ()
  (setq riece-unread-channels
	(delete riece-current-channel
		riece-unread-channels))
  (riece-unread-update-channel-list-buffer))

(defun riece-unread-update-channel-list-buffer ()
  (if riece-channel-list-buffer-mode
      (save-excursion
	(set-buffer riece-channel-list-buffer)
	(let ((inhibit-read-only t)
	      buffer-read-only)
	  (goto-char (point-min))
	  (while (not (eobp))
	    (if (looking-at "\\( ?[0-9]+:\\)\\([ !]\\)\\(.+\\)")
		(let ((channel (match-string 3)))
		  (replace-match
		   (concat "\\1"
			   (if (member channel riece-unread-channels)
			       "!"
			     " ")
			   "\\3"))))
	    (forward-line))))))
      
(defun riece-unread-switch-to-channel ()
  (interactive)
  (if (car riece-unread-channels)
      (riece-command-switch-to-channel (car riece-unread-channels))
    (error "No unread channel!")))

(defvar riece-command-mode-map)
(defvar riece-dialogue-mode-map)
(defvar riece-channel-list-mode-map)

(defun riece-unread-insinuate ()
  (add-hook 'riece-after-display-message-functions
	    'riece-unread-display-message-function)
  (add-hook 'riece-channel-switch-hook
	    'riece-unread-channel-switch-hook)
  (add-hook 'riece-update-buffers-hook
	    'riece-unread-update-channel-list-buffer)
  (define-key riece-command-mode-map
    "\C-c\C-u" 'riece-unread-switch-to-channel)
  (define-key riece-dialogue-mode-map
    "u" 'riece-unread-switch-to-channel)
  (define-key riece-channel-list-mode-map
    "u" 'riece-unread-switch-to-channel))

(provide 'riece-unread)

;;; riece-unread.el ends here
