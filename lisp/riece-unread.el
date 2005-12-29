;;; riece-unread.el --- mark channels where new messages arrived
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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; NOTE: This is an add-on module for Riece.

;; This add-on marks channels where new messages arrived.

;; You can check the unread channels via `C-c g' in the commands
;; buffer, by adding the following lines to ~/.riece/init.el:

;;   (add-hook 'riece-guess-channel-try-functions
;;             'riece-guess-channel-from-unread)

;;; Code:

(require 'riece-message)
(require 'riece-commands)
(require 'riece-signal)
(require 'riece-highlight)

(defgroup riece-unread nil
  "Mark unread channels."
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

(unless (riece-facep 'riece-modeline-unread-face)
  (make-face 'riece-modeline-unread-face
	     "Face used for displaying unread channels in modeline.")
  (if (featurep 'xemacs)
      (set-face-parent 'riece-modeline-unread-face 'modeline))
  (set-face-foreground 'riece-modeline-unread-face
		       (face-foreground 'riece-channel-list-unread-face)))

(defvar riece-unread-channels nil)

(defconst riece-unread-description
  "Mark channels where new messages arrived.")

(defun riece-unread-after-display-message-function (message)
  (if (get 'riece-unread 'riece-addon-enabled)
      (let ((target (if (riece-message-private-p message)
			(riece-message-speaker message)
		      (riece-message-target message))))
	(unless (or (riece-message-own-p message)
		    (riece-message-type message)
		    (riece-identity-equal target riece-current-channel)
		    (riece-identity-member target riece-unread-channels))
	  (setq riece-unread-channels (cons target riece-unread-channels))
	  (riece-emit-signal 'channel-list-changed)))))

(defun riece-unread-after-switch-to-channel-function (last)
  (if (get 'riece-unread 'riece-addon-enabled)
      (setq riece-unread-channels
	    (delq (car (riece-identity-member riece-current-channel
					      riece-unread-channels))
		  riece-unread-channels))))

(defun riece-unread-format-identity-for-channel-list-buffer (index identity)
  (if (and (get 'riece-unread 'riece-addon-enabled)
	   (riece-identity-member identity riece-unread-channels))
      (concat (format "%2d:!" index)
	      (riece-format-identity identity))))

(defun riece-unread-format-identity-for-channel-list-indicator (index identity)
  (if (and (get 'riece-unread 'riece-addon-enabled)
	   (riece-identity-member identity riece-unread-channels))
      (let ((string (riece-format-identity identity))
	    (start 0))
	;; Escape % -> %%.
	(while (string-match "%" string start)
	  (setq start (1+ (match-end 0))
		string (replace-match "%%" nil nil string)))
	(list (format "%d:" index)
	      (riece-propertize-modeline-string
	       string 'face 'riece-modeline-unread-face)))))

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
  (reverse riece-unread-channels))

(defun riece-unread-requires ()
  (let (requires)
    (if (memq 'riece-highlight riece-addons)
	(setq requires (cons 'riece-highlight requires)))
    ;; To override riece-history's channel mark in the channel list buffer.
    (if (memq 'riece-history riece-addons)
	(setq requires (cons 'riece-history requires)))
;;;    (if (memq 'riece-guess riece-addons)
;;;	(setq requires (cons 'riece-guess requires)))
    requires))

(defun riece-unread-insinuate ()
  (add-hook 'riece-after-display-message-functions
	    'riece-unread-after-display-message-function)
  (add-hook 'riece-after-switch-to-channel-functions
	    'riece-unread-after-switch-to-channel-function)
  (add-hook 'riece-format-identity-for-channel-list-buffer-functions
	    'riece-unread-format-identity-for-channel-list-buffer)
  (add-hook 'riece-format-identity-for-channel-list-indicator-functions
	    'riece-unread-format-identity-for-channel-list-indicator)
  (if (memq 'riece-highlight riece-addons)
      (setq riece-channel-list-mark-face-alist
	    (cons '(?! . riece-channel-list-unread-face)
		  riece-channel-list-mark-face-alist)))
;;;  (if (memq 'riece-guess riece-addons)
;;;      (add-hook 'riece-guess-channel-try-functions
;;;		'riece-guess-channel-from-unread))
  )

(defun riece-unread-uninstall ()
  (remove-hook 'riece-after-display-message-functions
	       'riece-unread-after-display-message-function)
  (remove-hook 'riece-after-switch-to-channel-functions
	       'riece-unread-after-switch-to-channel-function)
  (remove-hook 'riece-format-identity-for-channel-list-buffer-functions
	       'riece-unread-format-identity-for-channel-list-buffer)
  (remove-hook 'riece-format-identity-for-channel-list-indicator-functions
	       'riece-unread-format-identity-for-channel-list-indicator)
  (setq riece-channel-list-mark-face-alist
	(delq (assq ?! riece-channel-list-mark-face-alist)
	      riece-channel-list-mark-face-alist))
;;;  (if (memq 'riece-guess riece-addons)
;;;      (add-hook 'riece-guess-channel-try-functions
;;;		'riece-guess-channel-from-unread))
  )

(defvar riece-command-mode-map)
(defvar riece-dialogue-mode-map)
(defvar riece-channel-list-mode-map)
(defun riece-unread-enable ()
  (define-key riece-command-mode-map
    "\C-c\C-u" 'riece-unread-switch-to-channel)
  (define-key riece-dialogue-mode-map
    "u" 'riece-unread-switch-to-channel)
  (define-key riece-channel-list-mode-map
    "u" 'riece-unread-switch-to-channel)  
  (riece-emit-signal 'channel-list-changed))

(defun riece-unread-disable ()
  (define-key riece-command-mode-map
    "\C-c\C-u" nil)
  (define-key riece-dialogue-mode-map
    "u" nil)
  (define-key riece-channel-list-mode-map
    "u" nil)
  (setq riece-unread-channels nil)
  (riece-emit-signal 'channel-list-changed))

(provide 'riece-unread)

;;; riece-unread.el ends here
