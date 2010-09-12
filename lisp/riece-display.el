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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'riece-options)
(require 'riece-channel)
(require 'riece-misc)
(require 'riece-layout)
(require 'riece-signal)
(require 'riece-mcat)

(defvar riece-channel-buffer-format "*Channel:%s*"
  "Format of channel message buffer.")
(defvar riece-channel-buffer-alist nil
  "An alist mapping identities to channel buffers.")

(defvar riece-update-buffer-functions nil
  "Functions to redisplay the buffer.
Local to the buffer in `riece-buffer-list'.")

(defvar riece-update-indicator-functions
  '(riece-update-status-indicators
    riece-update-channel-status-indicator
    riece-update-channel-indicator
    riece-update-long-channel-indicator
    riece-update-channel-list-indicator)
  "Functions to update modeline indicators.")

(defun riece-display-connect-signals ()
  (riece-connect-signal
   'channel-list-changed
   (lambda (signal handback)
     (with-current-buffer riece-channel-list-buffer
       (run-hooks 'riece-update-buffer-functions))
     (riece-update-channel-list-indicator)))
  (riece-connect-signal
   'user-list-changed
   (lambda (signal handback)
     (with-current-buffer riece-user-list-buffer
       (run-hooks 'riece-update-buffer-functions)))
   (lambda (signal)
     (and riece-current-channel
	  (riece-identity-equal (car (riece-signal-args signal))
				riece-current-channel))))
  (riece-connect-signal
   'channel-switched
   (lambda (signal handback)
     (riece-update-status-indicators)
     (riece-update-channel-status-indicator)
     (riece-update-channel-indicator)
     (riece-update-long-channel-indicator)
     (force-mode-line-update t)
     (riece-emit-signal 'channel-list-changed)
     (riece-emit-signal 'user-list-changed riece-current-channel)
     (save-excursion
       (riece-redraw-layout))))
  (riece-connect-signal
   'user-joined-channel
   (lambda (signal handback)
     (riece-emit-signal 'user-list-changed riece-current-channel))
   (lambda (signal)
     (and riece-current-channel
	  (riece-identity-equal (nth 1 (riece-signal-args signal))
				riece-current-channel)
	  (not (riece-identity-equal (car (riece-signal-args signal))
				     (riece-current-nickname))))))
  (riece-connect-signal
   'user-joined-channel
   (lambda (signal handback)
     (riece-join-channel (nth 1 (riece-signal-args signal)))
     (riece-switch-to-channel (nth 1 (riece-signal-args signal)))
     (setq riece-join-channel-candidate nil))
   (lambda (signal)
     (riece-identity-equal (car (riece-signal-args signal))
			   (riece-current-nickname))))
  (riece-connect-signal
   'user-left-channel
   (lambda (signal handback)
     (riece-emit-signal 'user-list-changed riece-current-channel))
   (lambda (signal)
     (and riece-current-channel
	  (riece-identity-equal (nth 1 (riece-signal-args signal))
				riece-current-channel)
	  (not (riece-identity-equal (car (riece-signal-args signal))
				     (riece-current-nickname))))))
  (riece-connect-signal
   'user-left-channel
   (lambda (signal handback)
     (riece-part-channel (nth 1 (riece-signal-args signal))))
   (lambda (signal)
     (riece-identity-equal (car (riece-signal-args signal))
			   (riece-current-nickname))))
  (riece-connect-signal
   'user-renamed
   (lambda (signal handback)
     (riece-emit-signal 'user-list-changed riece-current-channel))
   (lambda (signal)
     (and riece-current-channel
	  (equal (riece-identity-server (nth 1 (riece-signal-args signal)))
		 (riece-identity-server riece-current-channel))
	  (riece-with-server-buffer (riece-identity-server
				     riece-current-channel)
	    (when (riece-channel-p (riece-identity-prefix
				    riece-current-channel))
	      (riece-identity-assoc
	       (riece-identity-prefix (nth 1 (riece-signal-args signal)))
	       (riece-channel-get-users (riece-identity-prefix
					 riece-current-channel))
	       t))))))
  (riece-connect-signal
   'user-renamed
   (lambda (signal handback)
     (riece-update-status-indicators)
     (riece-update-channel-indicator)
     (force-mode-line-update t))
   (lambda (signal)
     (riece-identity-equal (nth 1 (riece-signal-args signal))
			   (riece-current-nickname))))
  (riece-connect-signal
   'user-renamed
   (lambda (signal handback)
     (riece-switch-to-channel (nth 1 (riece-signal-args signal))))
   (lambda (signal)
     (and riece-current-channel
	  (riece-identity-equal (car (riece-signal-args signal))
				riece-current-channel))))
  (riece-connect-signal
   'user-renamed
   (lambda (signal handback)
     (let* ((old-identity (car (riece-signal-args signal)))
	    (new-identity (nth 1 (riece-signal-args signal)))
	    (pointer (riece-identity-member old-identity
					    riece-current-channels)))
       ;; Rename the channel buffer.
       (when pointer
	 (setcar pointer new-identity)
	 (with-current-buffer (riece-channel-buffer old-identity)
	   (rename-buffer (riece-channel-buffer-name new-identity) t)
	   (setq riece-channel-buffer-alist
		 (cons (cons new-identity (current-buffer))
		       (delq (riece-identity-assoc old-identity
						   riece-channel-buffer-alist)
			     riece-channel-buffer-alist))))))))
  (riece-connect-signal
   'user-away-changed
   (lambda (signal handback)
     (riece-update-status-indicators)
     (force-mode-line-update t))
   (lambda (signal)
     (riece-identity-equal (car (riece-signal-args signal))
			   (riece-current-nickname))))
  (riece-connect-signal
   'user-operator-changed
   (lambda (signal handback)
     (riece-update-status-indicators)
     (force-mode-line-update t))
   (lambda (signal)
     (riece-identity-equal (car (riece-signal-args signal))
			   (riece-current-nickname))))
  (riece-connect-signal
   'channel-topic-changed
   (lambda (signal handback)
     (riece-update-long-channel-indicator)
     (force-mode-line-update t))
   (lambda (signal)
     (and riece-current-channel
	  (riece-identity-equal (car (riece-signal-args signal))
				riece-current-channel))))
  (riece-connect-signal
   'channel-modes-changed
   (lambda (signal handback)
     (riece-update-long-channel-indicator)
     (force-mode-line-update t))
   (lambda (signal)
     (and riece-current-channel
	  (riece-identity-equal (car (riece-signal-args signal))
				riece-current-channel))))
  (riece-connect-signal
   'channel-operators-changed
   (lambda (signal handback)
     (riece-update-channel-status-indicator)
     (riece-emit-signal 'user-list-changed riece-current-channel))
   (lambda (signal)
     (and riece-current-channel
	  (riece-identity-equal (car (riece-signal-args signal))
				riece-current-channel))))
  (riece-connect-signal
   'channel-speakers-changed
   (lambda (signal handback)
     (riece-update-channel-status-indicator)
     (riece-emit-signal 'user-list-changed riece-current-channel))
   (lambda (signal)
     (and riece-current-channel
	  (riece-identity-equal (car (riece-signal-args signal))
				riece-current-channel))))
  (riece-connect-signal
   'buffer-freeze-changed
   (lambda (signal handback)
     (riece-update-status-indicators)
     (force-mode-line-update t))))

(defun riece-update-user-list-buffer ()
  (save-excursion
    (if (and riece-current-channel
	     (riece-channel-p (riece-identity-prefix riece-current-channel)))
	(let* ((users
		(riece-with-server-buffer (riece-identity-server
					   riece-current-channel)
		  (riece-channel-get-users (riece-identity-prefix
					    riece-current-channel))))
	       (inhibit-read-only t)
	       buffer-read-only)
	  (erase-buffer)
	  (riece-kill-all-overlays)
	  (while users
	    (insert (if (memq ?o (cdr (car users)))
			"@"
		      (if (memq ?v (cdr (car users)))
			  "+"
			" "))
		    (riece-format-identity
		     (riece-make-identity (car (car users))
					  (riece-identity-server
					   riece-current-channel))
		     t)
		    "\n")
	    (setq users (cdr users)))))))

(defun riece-format-identity-for-channel-list-buffer (index identity)
  (or (run-hook-with-args-until-success
       'riece-format-identity-for-channel-list-buffer-functions index identity)
      (concat (format "%2d:%c" index
		      (if (riece-identity-equal identity riece-current-channel)
			  ?*
			? ))
	      (riece-format-identity identity))))

(defun riece-update-channel-list-buffer ()
  (save-excursion
    (let ((inhibit-read-only t)
	  buffer-read-only
	  (index 1)
	  (channels riece-current-channels))
      (erase-buffer)
      (riece-kill-all-overlays)
      (while channels
	(if (car channels)
	    (insert (riece-format-identity-for-channel-list-buffer
		     index (car channels))
		    "\n"))
	(setq index (1+ index)
	      channels (cdr channels))))))

(defun riece-update-channel-indicator ()
  (setq riece-channel-indicator
	(if riece-current-channel
	    (riece-format-identity riece-current-channel)
	  (riece-mcat "None"))))

(defun riece-update-long-channel-indicator ()
  (setq riece-long-channel-indicator
	(if riece-current-channel
	    (if (riece-channel-p (riece-identity-prefix riece-current-channel))
		(riece-concat-channel-topic
		 riece-current-channel
		 (riece-concat-channel-modes
		  riece-current-channel
		  (riece-format-identity riece-current-channel)))
	      (riece-format-identity riece-current-channel))
	  (riece-mcat "None"))))

(defun riece-format-identity-for-channel-list-indicator (index identity)
  (or (run-hook-with-args-until-success
       'riece-format-identity-for-channel-list-indicator-functions
       index identity)
      (let ((string (riece-format-identity identity))
	    (start 0))
	;; Escape % -> %%.
	(while (string-match "%" string start)
	  (setq start (1+ (match-end 0))
		string (replace-match "%%" nil nil string)))
	(format "%d:%s" index string))))

(defun riece-update-channel-list-indicator ()
  (if (and riece-current-channels
	   ;; There is at least one channel.
	   (delq nil (copy-sequence riece-current-channels)))
      (let ((index 1)
	    pointer)
	(setq riece-channel-list-indicator
	      (delq
	       nil
	       (mapcar
		(lambda (channel)
		  (prog1
		      (if channel
			  (riece-format-identity-for-channel-list-indicator
			   index channel))
		    (setq index (1+ index))))
		riece-current-channels))
	      pointer riece-channel-list-indicator)
	(while pointer
	  (if (cdr pointer)
	      (setcdr pointer (cons "," (cdr pointer))))
	  (setq pointer (cdr (cdr pointer))))
	(setq riece-channel-list-indicator
	      (riece-normalize-modeline-string riece-channel-list-indicator)))
    (setq riece-channel-list-indicator (riece-mcat "No channel"))))

(defun riece-update-status-indicators ()
  (let ((server-name (riece-current-server-name)))
    (if server-name
	(with-current-buffer riece-command-buffer
	  (riece-with-server-buffer server-name
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
		  )
	    (if riece-real-nickname
		(setq riece-user-indicator
		      (riece-format-identity
		       (riece-make-identity riece-real-nickname
					    riece-server-name)
		       t)))))))
  (walk-windows
   (lambda (window)
     (with-current-buffer (window-buffer window)
       (if (riece-derived-mode-p 'riece-dialogue-mode)
	   (setq riece-freeze-indicator
		 (if (eq riece-freeze 'own)
		     "f"
		   (if riece-freeze
		       "F"
		     "-"))))))))

(defun riece-update-channel-status-indicator ()
  (if (and riece-current-channel
	   (riece-channel-p (riece-identity-prefix riece-current-channel)))
      (let ((users
	     (riece-with-server-buffer (riece-identity-server
					riece-current-channel)
	       (riece-channel-get-users (riece-identity-prefix
					 riece-current-channel))))
	    (nickname
	     (riece-with-server-buffer (riece-identity-server
					riece-current-channel)
	       riece-real-nickname)))
	(with-current-buffer riece-command-buffer
	  (setq riece-channel-status-indicator
		(if nickname
		    (let ((user (cdr (riece-identity-assoc nickname users t))))
		      (if (memq ?o user)
			  "@"
			(if (memq ?v user)
			    "+"
			  "-")))
		  "-"))))))

(defun riece-update-buffers (&optional buffers)
  (unless buffers
    (setq buffers riece-buffer-list))
  (while buffers
    (if (buffer-live-p (car buffers))
	(with-current-buffer (car buffers)
	  (run-hooks 'riece-update-buffer-functions)))
    (setq buffers (cdr buffers)))
  (run-hooks 'riece-update-indicator-functions)
  (force-mode-line-update t)
  (run-hooks 'riece-update-buffer-hook))

(defun riece-channel-buffer-name (identity)
  (let ((channels (riece-identity-member identity riece-current-channels)))
    (if channels
	(setq identity (car channels))
      (if riece-debug
	  (riece-debug (format "%S is not a member of riece-current-channels"
			       identity))))
    (format riece-channel-buffer-format (riece-format-identity identity))))

(eval-when-compile
  (autoload 'riece-channel-mode "riece"))
(defun riece-channel-buffer-create (identity)
  (with-current-buffer
      (riece-get-buffer-create (riece-channel-buffer-name identity)
			       'riece-channel-mode)
    (setq riece-channel-buffer-alist
	  (cons (cons identity (current-buffer))
		riece-channel-buffer-alist))
    (unless (eq major-mode 'riece-channel-mode)
      (riece-channel-mode)
      (let (buffer-read-only)
	(riece-insert-info (current-buffer)
			   (format (riece-mcat "Created on %s\n")
				   (funcall riece-format-time-function
					    (current-time))))
	(run-hook-with-args 'riece-channel-buffer-create-functions identity)))
    (current-buffer)))

(defun riece-channel-buffer (identity)
  (let ((entry (riece-identity-assoc identity riece-channel-buffer-alist)))
    (if entry
	(if (buffer-live-p (cdr entry))
	    (cdr entry)
	  (if riece-debug
	      (riece-debug
	       (format "riece-channel-buffer: nonexistent buffer: %s"
		       (riece-format-identity identity))))))))

(defun riece-switch-to-channel (identity)
  (let ((last riece-current-channel)
	window)
    (if (and riece-channel-buffer
	     (setq window (get-buffer-window riece-channel-buffer)))
	(with-current-buffer riece-channel-buffer
	  (setq riece-channel-buffer-window-point (window-point window))))
    (setq riece-current-channel identity
	  riece-channel-buffer (riece-channel-buffer riece-current-channel))
    (run-hook-with-args 'riece-after-switch-to-channel-functions last)
    (riece-emit-signal 'channel-switched)))

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
      (let ((last riece-current-channel))
	(run-hook-with-args 'riece-after-switch-to-channel-functions last)
	(setq riece-current-channel nil)
	(riece-emit-signal 'channel-switched)))))

(defun riece-part-channel (identity)
  (let ((pointer (riece-identity-member identity riece-current-channels)))
    (unless pointer
      (error "No such channel!"))
    (setcar pointer nil)
    (if (riece-identity-equal identity riece-current-channel)
	(riece-switch-to-nearest-channel pointer)
      (riece-emit-signal 'channel-list-changed))
    (funcall riece-buffer-dispose-function (riece-channel-buffer identity))))

(defun riece-redisplay-buffers (&optional force)
  (riece-update-buffers)
  (riece-redraw-layout force)
  (run-hooks 'riece-redisplay-buffers-hook))

(provide 'riece-display)

;;; riece-display.el ends here
