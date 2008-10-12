;;; riece-mini.el --- use Riece only on the minibuffer
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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; NOTE: This is an add-on module for Riece.

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
(require 'riece-biff)

(defgroup riece-mini nil
  "Use Riece only on the minibuffer."
  :group 'riece)

(defcustom riece-mini-backlog-size 5
  "*Line numbers for logging back log."
  :type 'integer
  :group 'riece-mini)

(defvar riece-mini-last-channel nil)
(defvar riece-mini-backlog-history nil)
(defvar riece-mini-backlog-shown nil)

(defconst riece-mini-description
  "Use Riece only on the minibuffer.")

(defun riece-mini-message-no-log (string &rest args)
  "Like `message', except that message logging is disabled."
  (if (featurep 'xemacs)
      (if args
	  (display-message 'no-log (apply #'format string args))
	(display-message 'no-log string))
    (let (message-log-max)
      (apply #'message string args))))

(defun riece-mini-display-message-function (message)
  "Show arrival messages to minibuffer."
  (let ((string (concat (format-time-string "%H:%M") " "
			(riece-format-message message t))))
    (when (string-match "\\(.*\\)$" string)
      (setq string (riece-match-string-no-properties 1 string)))
    (while (>= (length riece-mini-backlog-history)
	       riece-mini-backlog-size)
      (setq riece-mini-backlog-history
	    (cdr riece-mini-backlog-history)))
    (setq riece-mini-backlog-history
	  (reverse (cons string (reverse riece-mini-backlog-history))))
    (when (and (get 'riece-mini 'riece-addon-enabled)
	       (not (or (eq (window-buffer (selected-window))
			    (get-buffer riece-command-buffer))
			(riece-message-own-p message)
			(active-minibuffer-window))))
      (unless (riece-message-type message)
	(setq riece-mini-last-channel (riece-message-target message)))
      (riece-mini-message-no-log "%s" string))))

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
	 (message (read-string (format "Message to %s: "
				       (riece-format-identity target)))))
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
  (when riece-mini-backlog-history
    (let ((height (1+ riece-mini-backlog-size)))
      (mapc #'(lambda (string)
		(setq height (+ height
				(/ (string-width string) (window-width)))))
	    riece-mini-backlog-history)
      (let ((max-mini-window-height height)
	    (resize-mini-windows t))
	(setq riece-mini-backlog-shown t)
	(when (and (memq 'riece-biff riece-addons)
		   (get 'riece-biff 'riece-addon-enabled))
	  (riece-biff-clear))
	(riece-mini-message-no-log
	 "%s" (mapconcat #'identity riece-mini-backlog-history "\n"))))))

(defun riece-mini-pre-command ()
  (when riece-mini-backlog-shown
    (let ((resize-mini-windows t))
      (setq riece-mini-backlog-shown nil)
      (riece-mini-message-no-log ""))))

(defun riece-mini-requires ()
  (if (memq 'riece-biff riece-addons)
      '(riece-biff)))

(defun riece-mini-insinuate ()
  (add-hook 'riece-after-display-message-functions
	    'riece-mini-display-message-function)
  (add-hook 'pre-command-hook 'riece-mini-pre-command))

(defun riece-mini-uninstall ()
  (remove-hook 'riece-after-display-message-functions
	       'riece-mini-display-message-function)
  (remove-hook 'pre-command-hook 'riece-mini-pre-command))

(provide 'riece-mini)

;;; riece-mini.el ends here
