;;; riece-ignore.el --- ignore messages from some users
;; Copyright (C) 1998-2004 Daiki Ueno

;; Author: Daiki Ueno <ueno@unixuser.org>
;; Created: 1998-09-28
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

;;; Code:

(require 'riece-signal)
(require 'riece-identity)
(require 'riece-message)

(defgroup riece-ignore nil
  "Ignore messages from some users."
  :prefix "riece-"
  :group 'riece)

(defcustom riece-ignore-discard-message 'log
  "If t, messages from ignored user are completely discarded.
If 'log, messages are removed from IRC buffers, but they are saved in
`riece-ignore-buffer'.
Otherwise, they are not removed from IRC buffers, but are hidden with
'invisible text-property."
  :group 'riece-ignore
  :type '(choice (const :tag "Discard completely" t)
		 (const :tag "Discard but save logs" log)
		 (const :tag "Make messages invisible" nil)))

(defcustom riece-ignore-buffer-name "*Ignore*"
  "The name of buffer where ignored messages are stored."
  :group 'riece-ignore
  :type 'string)

(defcustom riece-startup-ignored-user-list nil
  "List of user names whose messages are ignored."
  :group 'riece-ignore
  :type '(repeat string))

(defvar riece-ignore-buffer nil)

(defconst riece-ignore-description
  "Ignore messages from some users.")
(defvar riece-ignored-user-list nil)

(defun riece-ignore-user-rename-signal-function (signal handback)
  (let ((pointer (riece-identity-member (car (riece-signal-args signal))
					riece-ignored-user-list)))
    (if pointer
	(setcar pointer (nth 1 (riece-signal-args signal))))))

(defun riece-ignore-user (user toggle)
  (interactive
   (let ((completion-ignore-case t))
     (list (if current-prefix-arg
	       (riece-completing-read-identity
		"Unignore user: "
		riece-ignored-user-list)
	     (riece-completing-read-identity
	      "Ignore user: "
	      (riece-get-users-on-server (riece-current-server-name))
	      (lambda (user)
		(not (riece-identity-member
		      (riece-parse-identity (car user))
		      riece-ignored-user-list)))))
	   (not current-prefix-arg))))
  (if toggle
      (progn
	(setq riece-ignored-user-list (cons user riece-ignored-user-list))
	(riece-connect-signal
	 'user-renamed
	 #'riece-ignore-user-rename-signal-function))
    (let ((pointer (riece-identity-member user riece-ignored-user-list)))
      (setq riece-ignored-user-list (delq (car pointer)
					  riece-ignored-user-list))
      (riece-disconnect-signal
       'user-renamed
       #'riece-ignore-user-rename-signal-function))))

(eval-when-compile
  (autoload 'riece-dialogue-mode "riece"))
(defun riece-ignore-message-filter (message)
  (if (and (get 'riece-ignore 'riece-addon-enabled)
	   (riece-identity-member (riece-message-speaker message)
				  riece-ignored-user-list))
      (if riece-ignore-discard-message
	  (when (eq riece-ignore-discard-message 'log)
	    (unless riece-ignore-buffer
	      (with-current-buffer (setq riece-ignore-buffer
					 (riece-get-buffer-create
					  riece-ignore-buffer-name
					  'riece-dialogue-mode))
		(riece-dialogue-mode)))
	    (with-current-buffer riece-ignore-buffer
	      (goto-char (point-max))
	      (let ((inhibit-read-only t)
		    buffer-read-only)
		(insert (concat (format-time-string "%H:%M") " "
				(riece-format-message message t))))))
	(put-text-property 0 (length (riece-message-text message))
			   'invisible 'riece-ignore
			   (riece-message-text message))
	message)
    message))

(defvar riece-command-mode-map)
(defun riece-ignore-insinuate ()
  (setq riece-ignored-user-list
	(mapcar #'riece-parse-identity riece-startup-ignored-user-list))
  (add-hook 'riece-message-filter-functions 'riece-ignore-message-filter))

(defun riece-ignore-uninstall ()
  (setq riece-ignored-user-list nil)
  (remove-hook 'riece-message-filter-functions 'riece-ignore-message-filter))

(defun riece-ignore-enable ()
  (define-key riece-command-mode-map
    "\C-ck" 'riece-ignore-user))

(defun riece-ignore-disable ()
  (define-key riece-command-mode-map
    "\C-ck" nil))

(provide 'riece-ignore)

;;; riece-ignore.el ends here