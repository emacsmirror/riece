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
;; (add-to-list 'riece-addons 'riece-mini t)
;;
;; And for using conveniently, bind any global key to
;; `riece-mini-send-message'.
;; For example:
;; (global-set-key "\C-cm" 'riece-mini-send-message)

;;; Code:

(eval-when-compile (require 'riece-message))

(defvar riece-mini-last-channel nil)

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
  (unless (or (eq (window-buffer (selected-window))
		  (get-buffer riece-command-buffer))
	      (riece-message-own-p message)
	      (active-minibuffer-window))
    (let ((open-bracket
	   (funcall riece-message-make-open-bracket-function message))
	  (close-bracket
	   (funcall riece-message-make-close-bracket-function message))
	  (global-name
	   (funcall riece-message-make-global-name-function message)))
      (setq riece-mini-last-channel (riece-message-target message))
      (riece-mini-message-no-log
       "%s" (concat (format-time-string "%H:%M") " "
		    open-bracket global-name close-bracket
		    " " (riece-message-text message))))))

(defun riece-mini-send-message (arg)
  "Send message using minibuffer.
Prefix argument onece (C-u), send message to last received channel.
If twice (C-u C-u), then ask the channel."
  (interactive "P")
  (let* ((completion-ignore-case t)
	 (target
	  (cond
	   ((equal arg '(16))
	    (completing-read "Channel/User: "
			     (mapcar #'list riece-current-channels) nil t))
	   (arg (or riece-mini-last-channel riece-current-channel))
	   (t riece-current-channel)))
	 (message (read-string (format "Message to %s: " target))))
    (unless (equal message "")
      (riece-switch-to-channel target)
      (riece-send-string
       (format "PRIVMSG %s :%s\r\n"
	       (riece-identity-prefix target)
	       message))
      (riece-own-channel-message message target))))

(defun riece-mini-insinuate ()
  (add-hook 'riece-after-display-message-functions
	    'riece-mini-display-message-function))

(provide 'riece-mini)

;;; riece-mini.el ends here
