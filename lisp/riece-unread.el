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

;; This add-on marks channels where new messages are arrived.

;; You can check the unread channels via `C-c g' in the commands
;; buffer, by adding the following lines to ~/.riece/init.el:

;;   (add-hook 'riece-guess-channel-try-functions
;;             'riece-guess-channel-from-unread)

;;; Code:

(require 'riece-message)
(require 'riece-commands)

(eval-when-compile (require 'riece-highlight))

(defgroup riece-unread nil
  "Mark unread channels"
  :tag "Unread"
  :prefix "riece-"
  :group 'riece)

(defface riece-channel-list-unread-face
  '((((class color)
      (background dark))
     (:foreground "orange"))
    (((class color)
      (background light))
     (:foreground "firebrick"))
    (t
     (:bold t)))
  "Face used for displaying unread channels."
  :group 'riece-highlight-faces)
(defvar riece-channel-list-unread-face 'riece-channel-list-unread-face)

(defvar riece-unread-channels nil)

(defun riece-unread-after-display-message-function (message)
  (unless (or (riece-message-own-p message)
	      (riece-identity-equal (riece-message-target message)
				    riece-current-channel)
	      (riece-identity-member (riece-message-target message)
				     riece-unread-channels))
    (setq riece-unread-channels
	  (cons (riece-message-target message) riece-unread-channels))
    (riece-redisplay-buffers)))

(defun riece-unread-after-switch-to-channel-function (last)
  (setq riece-unread-channels
	(delete riece-current-channel
		riece-unread-channels)))

(defun riece-unread-format-channel-list-line (index channel)
  (if (riece-identity-member channel riece-unread-channels)
      (concat (format "%2d:!" index)
	      (riece-format-identity channel)
	      "\n")))

(defun riece-unread-switch-to-channel ()
  (interactive)
  (if riece-unread-channels
      (let ((channel (car riece-unread-channels)))
	(if (riece-identity-member channel riece-current-channels)
	    (riece-command-switch-to-channel channel)
	  (setq riece-unread-channels
		(delete channel riece-unread-channels))
	  (riece-unread-switch-to-channel)))
    (error "No unread channel!")))

(defun riece-guess-channel-from-unread ()
  riece-unread-channels)

(defvar riece-command-mode-map)
(defvar riece-dialogue-mode-map)
(defvar riece-channel-list-mode-map)

(defun riece-unread-requires ()
  (let (requires)
    (if (memq 'riece-highlight riece-addons)
	(setq requires (cons 'riece-highlight requires)))
;;;    (if (memq 'riece-guess riece-addons)
;;;	(setq requires (cons 'riece-guess requires)))
    requires))

(defun riece-unread-insinuate ()
  (add-hook 'riece-after-display-message-functions
	    'riece-unread-after-display-message-function)
  (add-hook 'riece-after-switch-to-channel-functions
	    'riece-unread-after-switch-to-channel-function)
  (add-hook 'riece-format-channel-list-line-functions
	    'riece-unread-format-channel-list-line)
  (define-key riece-command-mode-map
    "\C-c\C-u" 'riece-unread-switch-to-channel)
  (define-key riece-dialogue-mode-map
    "u" 'riece-unread-switch-to-channel)
  (define-key riece-channel-list-mode-map
    "u" 'riece-unread-switch-to-channel)
  (if (memq 'riece-highlight riece-addons)
      (setq riece-channel-list-mark-face-alist
	    (cons '(?! . riece-channel-list-unread-face)
		  riece-channel-list-mark-face-alist)))
;;;  (if (memq 'riece-guess riece-addons)
;;;      (add-hook 'riece-guess-channel-try-functions
;;;		'riece-guess-channel-from-unread))
  )

(provide 'riece-unread)

;;; riece-unread.el ends here
