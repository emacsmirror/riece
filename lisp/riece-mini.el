;;; riece-mini.el --- "riece on minibuffer" add-on
;; Copyright (C) 2003 OHASHI Akira

;; Author: OHASHI Akira <bg66@koka-in.org>
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

;; This add-on shows arrival messages to minibuffer. And you can send
;; message using minibuffer.
;;
;; By using this add-on, you can use always "mini riece", even if you
;; are visiting other buffers.

;; To use, add the following line to your ~/.riece/init.el:
;; (add-to-list 'riece-addons 'riece-mini)
;;
;; For using conveniently, bind any global key to
;; `riece-mini-send-message' and `riece-mini-show-backlog'.
;; For example:
;; (global-set-key "\C-cm" 'riece-mini-send-message)
;; (global-set-key "\C-cb" 'riece-mini-show-backlog)

;;; Code:

(require 'riece-message)

(defgroup riece-mini nil
  "riece on minibuffer"
  :group 'riece)

(defcustom riece-mini-backlog-size 10
  "*Line numbers for logging back log."
  :type 'integer
  :group 'riece-mini)

(defvar riece-mini-last-channel nil)
(defvar riece-mini-backlog-history nil)
(defvar riece-mini-backlog-shown nil)

(defvar riece-mini-enabled nil)

(defconst riece-mini-description
  "Send arrival messages to minibuffer")

(defmacro riece-mini-message-no-log (string &rest args)
  "Like `message', except that message logging is disabled."
  (if (featurep 'xemacs)
      (if args
	  `(display-message 'no-log (format ,string ,@args))
	`(display-message 'no-log ,string))
    `(let (message-log-max)
       (message ,string ,@args))))

(defun riece-mini-display-message-function (message)
  "Show arrival messages to minibuffer."
  (when (and riece-mini-enabled
	     (not (or (eq (window-buffer (selected-window))
			  (get-buffer riece-command-buffer))
		      (riece-message-own-p message)
		      (active-minibuffer-window))))
    (unless (riece-message-type message)
      (setq riece-mini-last-channel (riece-message-target message)))
    (let ((string (concat (format-time-string "%H:%M") " "
			  (riece-format-message message t))))
      (riece-mini-message-no-log "%s" string)
      (when (>= (length riece-mini-backlog-history)
		riece-mini-backlog-size)
	(pop riece-mini-backlog-history))
      (push string riece-mini-backlog-history))))

(defun riece-mini-send-message (arg)
  "Send message using minibuffer.
Prefix argument onece (C-u), send message to last received channel.
If twice (C-u C-u), then ask the channel."
  (interactive "P")
  (let* ((completion-ignore-case t)
	 (target
	  (cond
	   ((equal arg '(16))
	    (riece-completing-read-identity
	     "Channel/User: " riece-current-channels nil t))
	   (arg (or riece-mini-last-channel riece-current-channel))
	   (t riece-current-channel)))
	 (message (read-string (format "Message to %s: " target))))
    (unless (equal message "")
      (riece-switch-to-channel target)
      (riece-send-string
       (format "PRIVMSG %s :%s\r\n"
	       (riece-identity-prefix target)
	       message))
      (riece-display-message
       (riece-make-message (riece-current-nickname) target
			   message nil t)))))

(defun riece-mini-show-backlog ()
  "Send back logs to minibuffer."
  (interactive)
  (let ((max-mini-window-height riece-mini-backlog-size)
	(resize-mini-windows t))
    (when riece-mini-backlog-history
      (setq riece-mini-backlog-shown t)
      (riece-mini-message-no-log
       (mapconcat 'identity (reverse riece-mini-backlog-history) "")))))

(defun riece-mini-pre-command ()
  (when riece-mini-backlog-shown
    (let ((resize-mini-windows t))
      (setq riece-mini-backlog-shown nil)
      (riece-mini-message-no-log ""))))

(defun riece-mini-insinuate ()
  (add-hook 'riece-after-display-message-functions
	    'riece-mini-display-message-function)
  (add-hook 'pre-command-hook 'riece-mini-pre-command))

(defun riece-mini-enable ()
  (setq riece-mini-enabled t))

(defun riece-mini-disable ()
  (setq riece-mini-enabled nil))

(provide 'riece-mini)

;;; riece-mini.el ends here
