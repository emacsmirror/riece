;;; riece-display.el --- buffer arrangement
;; Copyright (C) 1998-2003 Daiki Ueno

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
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(require 'riece-options)
(require 'riece-channel)
(require 'riece-misc)
(require 'riece-layout)

(defvar riece-update-buffer-functions
  '(riece-update-user-list-buffer
    riece-update-channel-list-buffer
    riece-update-status-indicators
    riece-update-channel-indicator
    riece-update-short-channel-indicator
    riece-update-channel-list-indicator))

(defvar riece-channel-list-changed nil)

(defun riece-update-user-list-buffer ()
  (save-excursion
    (set-buffer riece-user-list-buffer)
    (when (and riece-current-channel
	       (riece-channel-p (riece-identity-prefix riece-current-channel)))
      (let (users operators speakers)
	(with-current-buffer (process-buffer (riece-server-process
					      (riece-identity-server
					       riece-current-channel)))
	  (setq users
		(riece-channel-get-users
		 (riece-identity-prefix riece-current-channel))
		operators
		(riece-channel-get-operators
		 (riece-identity-prefix riece-current-channel))
		speakers
		(riece-channel-get-speakers
		 (riece-identity-prefix riece-current-channel))))
	(let ((inhibit-read-only t)
	      buffer-read-only)
	  (erase-buffer)
	  (while users
	    (if (member (car users) operators)
		(insert "@" (car users) "\n")
	      (if (member (car users) speakers)
		  (insert "+" (car users) "\n")
		(insert " " (car users) "\n")))
	    (setq users (cdr users))))))))

(defun riece-update-channel-list-buffer ()
  (if riece-channel-list-changed
      (save-excursion
	(set-buffer riece-channel-list-buffer)
	(let ((inhibit-read-only t)
	      buffer-read-only
	      (index 1)
	      (channels riece-current-channels))
	  (erase-buffer)
	  (while channels
	    (if (car channels)
		(let ((point (point)))
		  (insert (format "%2d: " index)
			  (riece-format-identity (car channels))
			  "\n")))
	    (setq index (1+ index)
		  channels (cdr channels)))))))

(defun riece-update-channel-indicator ()
  (setq riece-channel-indicator
	(if riece-current-channel
	    (if (riece-channel-p (riece-identity-prefix riece-current-channel))
		(riece-concat-channel-modes
		 riece-current-channel
		 (riece-concat-channel-topic
		  riece-current-channel
		  (riece-format-identity riece-current-channel)))
	      (riece-format-identity riece-current-channel))
	  "None")))

(defun riece-update-short-channel-indicator ()
  (setq riece-short-channel-indicator
	(if riece-current-channel
	    (riece-format-identity riece-current-channel)
	  "None")))

(defun riece-update-channel-list-indicator ()
  (if riece-channel-list-changed
      (if (and riece-current-channels
	       ;; There is at least one channel.
	       (delq nil (copy-sequence riece-current-channels)))
	  (let ((index 1))
	    (setq riece-channel-list-indicator
		  (mapconcat
		   #'identity
		   (delq nil
			 (mapcar
			  (lambda (channel)
			    (prog1
				(if channel
				    (format "%d:%s" index
					    (riece-format-identity channel)))
			      (setq index (1+ index))))
			  riece-current-channels))
		   ",")))
	(setq riece-channel-list-indicator "No channel"))))

(defun riece-update-status-indicators ()
  (if riece-current-channel
      (with-current-buffer riece-command-buffer
	(riece-with-server-buffer (riece-identity-server riece-current-channel)
	  (setq riece-away-indicator
		(if (and riece-real-nickname
			 (riece-user-get-away riece-real-nickname))
		    "A"
		  "-")
		riece-operator-indicator
		(if (and riece-real-nickname
			 (riece-user-get-operator riece-real-nickname))
		    "O"
		  "-")
		riece-user-indicator riece-real-nickname))))
  (setq riece-freeze-indicator
	(with-current-buffer (if (and riece-channel-buffer-mode
				      riece-channel-buffer)
				 riece-channel-buffer
			       riece-dialogue-buffer)
	  (if (eq riece-freeze 'own)
	      "f"
	    (if riece-freeze
		"F"
	      "-")))))

(defun riece-update-buffers ()
  (if riece-current-channel
      (setq riece-channel-buffer (get-buffer (riece-channel-buffer-name
					      riece-current-channel))))
  (run-hooks 'riece-update-buffer-functions)
  (setq riece-channel-list-changed nil)
  (force-mode-line-update t))

(defun riece-channel-buffer-name (identity)
  (let ((channels (riece-identity-member identity riece-current-channels)))
    (if channels
	(setq identity (car channels))
      (if riece-debug
	  (message "%S is not a member of riece-current-channels" identity)))
    (format riece-channel-buffer-format (riece-format-identity identity))))

(eval-when-compile
  (autoload 'riece-channel-mode "riece"))
(defun riece-channel-buffer-create (identity)
  (with-current-buffer
      (riece-get-buffer-create (riece-channel-buffer-name identity))
    (unless (eq major-mode 'riece-channel-mode)
      (riece-channel-mode)
      (let (buffer-read-only)
	(riece-insert-info (current-buffer)
			   (concat "Created on "
				   (funcall riece-format-time-function
					    (current-time))
				   "\n"))
	(run-hook-with-args 'riece-channel-buffer-create-functions identity)))
    (current-buffer)))

(defun riece-switch-to-channel (identity)
  (let ((last riece-current-channel))
    (setq riece-current-channel identity)
    (run-hook-with-args 'riece-after-switch-to-channel-functions last)))

(defun riece-join-channel (identity)
  (unless (riece-identity-member identity riece-current-channels)
    (setq riece-current-channels
	  (riece-identity-assign-binding
	   identity riece-current-channels
	   (mapcar
	    (lambda (channel)
	      (if channel
		  (riece-parse-identity channel)))
	    riece-default-channel-binding)))
    (riece-channel-buffer-create identity)
    (setq riece-channel-list-changed t)))

(defun riece-switch-to-nearest-channel (pointer)
  (let ((start riece-current-channels)
	identity)
    (while (and start (not (eq start pointer)))
      (if (car start)
	  (setq identity (car start)))
      (setq start (cdr start)))
    (unless identity
      (while (and pointer
		  (null (car pointer)))
	(setq pointer (cdr pointer)))
      (setq identity (car pointer)))
    (if identity
	(riece-switch-to-channel identity)
      (let ((last riece-current-channel))
	(run-hook-with-args 'riece-after-switch-to-channel-functions last)
	(setq riece-current-channel nil)))))

(defun riece-part-channel (identity)
  (let ((pointer (riece-identity-member identity riece-current-channels)))
    (if pointer
	(setcar pointer nil))
    (if (riece-identity-equal identity riece-current-channel)
	(riece-switch-to-nearest-channel pointer))
    (setq riece-channel-list-changed t)))

(defun riece-redisplay-buffers (&optional force)
  (riece-update-buffers)
  (riece-redraw-layout force)
  (run-hooks 'riece-redisplay-buffers-hook))

(provide 'riece-display)

;;; riece-display.el ends here
