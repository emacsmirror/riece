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

(defvar riece-update-buffer-functions
  '(riece-update-user-list-buffer
    riece-update-channel-list-buffer
    riece-update-status-indicators
    riece-update-channel-indicator
    riece-update-short-channel-indicator
    riece-update-channel-list-indicator))

(defcustom riece-configure-windows-function #'riece-configure-windows
  "Function to configure windows."
  :type 'function
  :group 'riece-looks)

(defcustom riece-configure-windows-predicate
  #'riece-configure-windows-predicate
  "Function to check whether window reconfiguration is needed."
  :type 'function
  :group 'riece-looks)

(defun riece-configure-windows ()
  (let ((buffer (window-buffer))
	(show-user-list
	 (and riece-user-list-buffer-mode
	      riece-current-channel
	      ;; User list buffer is nuisance for private conversation.
	      (riece-channel-p (riece-identity-prefix
				riece-current-channel)))))
    ;; Can't expand minibuffer to full frame.
    (if (eq (selected-window) (minibuffer-window))
	(other-window 1))
    (delete-other-windows)
    (if (and riece-current-channel
	     (or show-user-list riece-channel-list-buffer-mode))
	(let ((rest-window (split-window (selected-window)
					 (/ (window-width) 5) t)))
	  (if (and show-user-list riece-channel-list-buffer-mode)
	      (progn
		(set-window-buffer (split-window)
				   riece-channel-list-buffer)
		(set-window-buffer (selected-window)
				   riece-user-list-buffer))
	    (if show-user-list
		(set-window-buffer (selected-window)
				   riece-user-list-buffer)
	      (if riece-channel-list-buffer-mode
		  (set-window-buffer (selected-window)
				     riece-channel-list-buffer))))
	  (select-window rest-window)))
    (if (and riece-current-channel
	     riece-channel-buffer-mode)
	(let ((rest-window (split-window)))
	  (set-window-buffer (selected-window)
			     riece-channel-buffer)
	  (set-window-buffer (split-window rest-window 4)
			     riece-others-buffer)
	  (with-current-buffer riece-channel-buffer
	    (setq truncate-partial-width-windows nil))
	  (with-current-buffer riece-others-buffer
	    (setq truncate-partial-width-windows nil))
	  (set-window-buffer rest-window
			     riece-command-buffer))
      (set-window-buffer (split-window (selected-window) 4)
			 riece-dialogue-buffer)
      (set-window-buffer (selected-window)
			 riece-command-buffer))
    (riece-set-window-points)
    (select-window (or (get-buffer-window buffer)
		       (get-buffer-window riece-command-buffer)))))

(defun riece-set-window-points ()
  (if (and riece-user-list-buffer
	   (get-buffer-window riece-user-list-buffer))
      (with-current-buffer riece-user-list-buffer
	(unless (riece-frozen riece-user-list-buffer)
	  (set-window-start (get-buffer-window riece-user-list-buffer)
			    (point-min)))))
  (if (and riece-channel-list-buffer
	   (get-buffer-window riece-channel-list-buffer))
      (with-current-buffer riece-channel-list-buffer
	(unless (riece-frozen riece-channel-list-buffer)
	  (set-window-start (get-buffer-window riece-channel-list-buffer)
			    (point-min))))))

(defun riece-update-user-list-buffer ()
  (if (and riece-user-list-buffer
	   (get-buffer riece-user-list-buffer)
	   riece-current-channel
	   (riece-channel-p (riece-identity-prefix riece-current-channel)))
      (save-excursion
	(set-buffer (process-buffer (riece-server-process
				     (riece-identity-server
				      riece-current-channel))))
	(let* ((inhibit-read-only t)
	       buffer-read-only
	       (channel (riece-identity-prefix riece-current-channel))
	       (users (riece-channel-get-users channel))
	       (operators (riece-channel-get-operators channel))
	       (speakers (riece-channel-get-speakers channel)))
	  (set-buffer riece-user-list-buffer)
	  (erase-buffer)
	  (while users
	    (if (member (car users) operators)
		(insert "@" (car users) "\n")
	      (if (member (car users) speakers)
		  (insert "+" (car users) "\n")
		(insert " " (car users) "\n")))
	    (setq users (cdr users)))))))

(defun riece-update-channel-list-buffer ()
  (if (and riece-channel-list-buffer
	   (get-buffer riece-channel-list-buffer))
      (save-excursion
	(set-buffer riece-channel-list-buffer)
	(let ((inhibit-read-only t)
	      buffer-read-only
	      (index 1)
	      (channels riece-current-channels))
	  (erase-buffer)
	  (while channels
	    (if (car channels)
		(insert (format "%2d: %s\n" index
				(riece-decode-identity (car channels)))))
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
		  (riece-decode-identity riece-current-channel)))
	      riece-current-channel)
	  "None")))

(defun riece-update-short-channel-indicator ()
  (setq riece-short-channel-indicator
	(if riece-current-channel
	    (riece-decode-identity riece-current-channel)
	  "None")))

(defun riece-update-channel-list-indicator ()
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
			(prog1 (if channel
				   (format "%d:%s" index
					   (riece-decode-identity channel)))
			  (setq index (1+ index))))
		      riece-current-channels))
	       ",")))
    (setq riece-channel-list-indicator "No channel")))

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
  (run-hooks 'riece-update-buffer-functions)
  (force-mode-line-update t)
  (run-hooks 'riece-update-buffers-hook))

(defun riece-channel-buffer-name (identity)
  (format riece-channel-buffer-format (riece-decode-identity identity)))

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
				   "\n"))))
    (current-buffer)))

(defun riece-switch-to-channel (identity)
  (setq riece-last-channel riece-current-channel
	riece-current-channel identity
	riece-channel-buffer
	(get-buffer (riece-channel-buffer-name identity)))
  (run-hooks 'riece-channel-switch-hook))

(defun riece-join-channel (identity)
  (unless (riece-identity-member identity riece-current-channels)
    (setq riece-current-channels
	  (riece-identity-assign-binding identity riece-current-channels
					 riece-default-channel-binding))
    (riece-channel-buffer-create identity)))

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
      (setq riece-last-channel riece-current-channel
	    riece-current-channel nil))))

(defun riece-part-channel (identity)
  (let ((pointer (riece-identity-member identity riece-current-channels)))
    (if pointer
	(setcar pointer nil))
    (if (riece-identity-equal identity riece-current-channel)
	(riece-switch-to-nearest-channel pointer))))

(defun riece-configure-windows-predicate ()
  ;; The current channel is changed, and some buffers are visible.
  (unless (equal riece-last-channel riece-current-channel)
    (let ((buffers riece-buffer-list))
      (catch 'found
	(while buffers
	  (if (and (buffer-live-p (car buffers))
		   (get-buffer-window (car buffers)))
	      (throw 'found t)
	    (setq buffers (cdr buffers))))))))

(defun riece-redisplay-buffers (&optional force)
  (riece-update-buffers)
  (if (or force
	  (funcall riece-configure-windows-predicate))
      (funcall riece-configure-windows-function))
  (run-hooks 'riece-redisplay-buffers-hook))

(provide 'riece-display)

;;; riece-display.el ends here
