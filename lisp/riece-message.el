;;; riece-message.el --- generate and display message line
;; Copyright (C) 1999-2003 Daiki Ueno

;; Author: Daiki Ueno <ueno@unixuser.org>
;; Keywords: message

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

(require 'riece-identity)
(require 'riece-channel)
(require 'riece-user)
(require 'riece-display)
(require 'riece-misc)

(defgroup riece-message nil
  "Display messages."
  :tag "Message"
  :prefix "riece-"
  :group 'riece)

(defcustom riece-message-filter-functions nil
  "Functions to filter incoming messages."
  :type 'function
  :group 'riece-message)

(defcustom riece-message-make-open-bracket-function
  #'riece-message-make-open-bracket
  "Function which makes `open-bracket' string for each message."
  :type 'function
  :group 'riece-message)

(defcustom riece-message-make-close-bracket-function
  #'riece-message-make-close-bracket
  "Function which makes `close-bracket' string for each message."
  :type 'function
  :group 'riece-message)

(defcustom riece-message-make-name-function
  #'riece-message-make-name
  "Function which makes local identity for each message."
  :type 'function
  :group 'riece-message)

(defcustom riece-message-make-global-name-function
  #'riece-message-make-global-name
  "Function which makes global identity for each message."
  :type 'function
  :group 'riece-message)

(defcustom riece-message-format-function-alist nil
  "Alist mapping message types to format functions."
  :type 'list
  :group 'riece-message)

(defun riece-message-make-open-bracket (message)
  "Make `open-bracket' string for MESSAGE."
  (if (eq (riece-message-type message) 'notice)
      "{"
    (if (riece-message-own-p message)
	">"
      (if (riece-message-private-p message)
	  "="
	(if (riece-message-external-p message)
	    "("
	  "<")))))

(defun riece-message-make-close-bracket (message)
  "Make `close-bracket' string for MESSAGE."
  (if (eq (riece-message-type message) 'notice)
      "}"
    (if (riece-message-own-p message)
	"<"
      (if (riece-message-private-p message)
	  "="
	(if (riece-message-external-p message)
	    ")"
	  ">")))))

(defun riece-message-make-name (message)
  "Make local identity for MESSAGE."
  (if (riece-message-private-p message)
      (if (riece-message-own-p message)
	  (riece-format-identity (riece-message-target message) t)
	(riece-format-identity (riece-message-speaker message) t))
    (riece-format-identity (riece-message-speaker message) t)))

(defun riece-message-make-global-name (message)
  "Make global identity for MESSAGE."
  (if (riece-message-private-p message)
      (if (riece-message-own-p message)
	  (riece-format-identity (riece-message-target message) t)
	(riece-format-identity (riece-message-speaker message) t))
    (concat (riece-format-identity (riece-message-target message) t) ":"
	    (riece-format-identity (riece-message-speaker message) t))))

(defun riece-message-buffer (message)
  "Return the buffer where MESSAGE should appear."
  (let ((target (if (riece-message-private-p message)
		    (if (riece-message-own-p message)
			(riece-message-target message)
		      (riece-message-speaker message))
		  (riece-message-target message))))
    (unless (riece-identity-member target riece-current-channels)
      (riece-join-channel target)
      ;; If you are not joined to any channel,
      ;; switch to the target immediately.
      (unless riece-current-channel
	(riece-switch-to-channel target)))
    (riece-channel-buffer target)))

(defun riece-message-parent-buffers (message buffer)
  "Return the parents of BUFFER where MESSAGE should appear.
Normally they are *Dialogue* and/or *Others*."
  (if (and buffer (riece-frozen buffer)) ;the message might not be
					 ;visible in buffer's window
      (list riece-dialogue-buffer riece-others-buffer)
    (if (and riece-current-channel	;the message is not sent to
					;the current channel
	     (if (riece-message-private-p message)
		 (if (riece-message-own-p message)
		     (not (riece-identity-equal
			   (riece-message-target message)
			   riece-current-channel))
		   (not (riece-identity-equal
			 (riece-message-speaker message)
			 riece-current-channel)))
	       (not (riece-identity-equal
		     (riece-message-target message)
		     riece-current-channel))))
	(list riece-dialogue-buffer riece-others-buffer)
      riece-dialogue-buffer)))

(defun riece-format-message-1 (message &optional global)
  (let ((open-bracket
	 (funcall riece-message-make-open-bracket-function message))
	(close-bracket
	 (funcall riece-message-make-close-bracket-function message))
	(name
	 (if global
	     (funcall riece-message-make-global-name-function message)
	   (funcall riece-message-make-name-function message)))
	(server-name (riece-identity-server (riece-message-speaker message))))
    (riece-with-server-buffer server-name
      (concat
       (if global
	   (riece-concat-server-name
	    (concat open-bracket name close-bracket
		    " " (riece-message-text message)))
	 (concat open-bracket name close-bracket
		 " " (riece-message-text message)))
       "\n"))))

(defun riece-format-message (message &optional global)
  (funcall (or (cdr (assq (riece-message-type message)
			  riece-message-format-function-alist))
	       #'riece-format-message-1)
	   message global))

(defun riece-display-message-1 (message)
  (let ((buffer (riece-message-buffer message))
	parent-buffers)
    (when (and buffer
	       (riece-message-own-p message)
	       (riece-own-frozen buffer))
      (with-current-buffer buffer
	(setq riece-freeze nil))
      (riece-emit-signal 'buffer-freeze-changed buffer nil))
    (setq parent-buffers (riece-message-parent-buffers message buffer))
    (riece-insert buffer (riece-format-message message))
    (riece-insert parent-buffers (riece-format-message message t))
    (with-current-buffer buffer
      (run-hook-with-args 'riece-after-display-message-functions message))))

(defun riece-display-message (message)
  "Display MESSAGE object."
  (let ((functions riece-message-filter-functions))
    (setq message (copy-sequence message))
    (while (and functions message)
      (setq message (funcall (car functions) message)
	    functions (cdr functions)))
    (if message
	(riece-display-message-1 message))))

(defun riece-make-message (speaker target text &optional type own-p)
  "Make an instance of message object.
Arguments are appropriate to the sender, the receiver, and text
content, respectively.
Optional 4th argument TYPE specifies the type of the message.
Currently possible values are `nil' or `notice'.
Optional 5th argument is the flag to indicate that this message is not
from the network."
  (vector speaker target text type own-p))

(defun riece-message-speaker (message)
  "Return the sender of MESSAGE."
  (aref message 0))

(defun riece-message-target (message)
  "Return the receiver of MESSAGE."
  (aref message 1))

(defun riece-message-text (message)
  "Return the text part of MESSAGE."
  (aref message 2))

(defun riece-message-type (message)
  "Return the type of MESSAGE.
Currently possible values are `action' and `notice'."
  (aref message 3))

(defun riece-message-own-p (message)
  "Return t if MESSAGE is not from the network."
  (aref message 4))

(defun riece-message-set-speaker (message speaker)
  "Set the sender of MESSAGE."
  (aset message 0 speaker))

(defun riece-message-set-target (message target)
  "Set the receiver of MESSAGE."
  (aset message 1 target))

(defun riece-message-set-text (message text)
  "Set the text part of MESSAGE."
  (aset message 2 text))

(defun riece-message-set-type (message type)
  "Set the type of MESSAGE.
Currently possible values are `action' and `notice'."
  (aset message 3 type))

(defun riece-message-set-own-p (message own-p)
  "Set t if MESSAGE is not from the network."
  (aset message 4 own-p))

(defun riece-message-private-p (message)
  "Return t if MESSAGE is a private message."
  (not (or (riece-channel-p (riece-identity-prefix
			     (riece-message-speaker message)))
	   (riece-channel-p (riece-identity-prefix
			     (riece-message-target message))))))

(defun riece-message-external-p (message)
  "Return t if MESSAGE is from outside the channel."
  (not (riece-identity-member
	(riece-message-speaker message)
	(let ((target (riece-message-target message)))
	  (riece-with-server-buffer (riece-identity-server target)
	    (mapcar
	     (lambda (user)
	       (riece-make-identity (car user) riece-server-name))
	     (riece-channel-get-users (riece-identity-prefix target))))))))

(provide 'riece-message)

;;; riece-message.el ends here
