;;; riece-ctcp.el --- CTCP (Client To Client Protocol) support
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

;;; Commentary:

;; NOTE: This is an add-on module for Riece.

;;; Code:

(require 'riece-version)
(require 'riece-misc)
(require 'riece-highlight)
(require 'riece-display)
(require 'riece-debug)
(require 'riece-mcat)
(require 'riece-message)

(defface riece-ctcp-action-face
  '((((class color)
      (background dark))
     (:foreground "PaleGreen" :italic t))
    (((class color)
      (background light))
     (:foreground "ForestGreen" :italic t))
    (t
     (:bold t)))
  "Face used for displaying \"*** Action:\" line"
  :group 'riece-highlight-faces)
(defvar riece-ctcp-action-face 'riece-ctcp-action-face)

(defconst riece-ctcp-action-prefix "*** Action: ")

(defvar riece-ctcp-ping-time nil)
(defvar riece-ctcp-additional-clientinfo nil)

(defvar riece-dialogue-mode-map)

(defconst riece-ctcp-description
  "CTCP (Client To Client Protocol) support.")

(defun riece-handle-ctcp-request (prefix string)
  (when (and (get 'riece-ctcp 'riece-addon-enabled) prefix string
	     (riece-prefix-nickname prefix))
    (let* ((parameters (riece-split-parameters string))
	   (targets (split-string (car parameters) ","))
	   (message (nth 1 parameters)))
      (if (string-match "\1\\([^ ]+\\)\\( .+\\)?\1" message)
	  (let ((request (downcase (match-string 1 message))))
	    (if (match-beginning 2)
		(setq message (substring (match-string 2 message) 1)))
	    (let ((hook
		   (intern (concat "riece-ctcp-" request "-request-hook")))
		  (function
		   (intern-soft (concat "riece-handle-ctcp-" request
					"-request")))
		  (after-hook
		   (intern (concat "riece-ctcp-after-" request
				   "-request-hook"))))
	      (unless (riece-funcall-ignore-errors
		       (symbol-name hook)
		       #'run-hook-with-args-until-success
		       hook prefix (car targets) message)
		(if function
		    (riece-funcall-ignore-errors (symbol-name function)
						  function prefix (car targets)
						  message))
		(riece-funcall-ignore-errors (symbol-name after-hook)
					     #'run-hook-with-args-until-success
					     after-hook prefix (car targets)
					     message)))
	    t)))))

(defun riece-handle-ctcp-version-request (prefix target string)
  (let* ((target-identity (riece-make-identity target riece-server-name))
	 (buffer (if (riece-channel-p target)
		     (riece-channel-buffer target-identity)))
	 (user (riece-prefix-nickname prefix)))
    (riece-send-string
     (format "NOTICE %s :\1VERSION %s\1\r\n" user (riece-extended-version)))
    (riece-insert-change buffer (format "CTCP VERSION from %s\n" user))
    (riece-insert-change
     (if (and riece-channel-buffer-mode
	      (not (eq buffer riece-channel-buffer)))
	 (list riece-dialogue-buffer riece-others-buffer)
       riece-dialogue-buffer)
     (concat
      (riece-concat-server-name
       (format (riece-mcat "CTCP VERSION from %s (%s) to %s")
	       user
	       (riece-strip-user-at-host (riece-prefix-user-at-host prefix))
	       (riece-format-identity target-identity t)))
      "\n"))))

(defun riece-handle-ctcp-ping-request (prefix target string)
  (let* ((target-identity (riece-make-identity target riece-server-name))
	 (buffer (if (riece-channel-p target)
		     (riece-channel-buffer target-identity)))
	 (user (riece-prefix-nickname prefix)))
    (riece-send-string
     (if string
	 (format "NOTICE %s :\1PING %s\1\r\n" user string)
       (format "NOTICE %s :\1PING\1\r\n" user)))
    (riece-insert-change buffer (format "CTCP PING from %s\n" user))
    (riece-insert-change
     (if (and riece-channel-buffer-mode
	      (not (eq buffer riece-channel-buffer)))
	 (list riece-dialogue-buffer riece-others-buffer)
       riece-dialogue-buffer)
     (concat
      (riece-concat-server-name
       (format (riece-mcat "CTCP PING from %s (%s) to %s")
	       user
	       (riece-strip-user-at-host (riece-prefix-user-at-host prefix))
	       (riece-format-identity target-identity t)))
      "\n"))))

(defun riece-handle-ctcp-clientinfo-request (prefix target string)
  (let* ((target-identity (riece-make-identity target riece-server-name))
	 (buffer (if (riece-channel-p target)
		     (riece-channel-buffer target-identity)))
	 (user (riece-prefix-nickname prefix)))
    (riece-send-string
     (format "NOTICE %s :\1CLIENTINFO %s\1\r\n"
	     user
	     (let (messages)
	       (mapatoms
		(lambda (atom)
		  (let ((case-fold-search t))
		    (if (and (fboundp atom)
			     (string-match
			      "riece-handle-ctcp-\\(.+\\)-request"
			      (symbol-name atom)))
			(setq messages
			      (cons (match-string 1 (symbol-name atom))
				    messages))))))
	       (mapconcat #'upcase (append messages
					   riece-ctcp-additional-clientinfo)
			  " "))))
    (riece-insert-change buffer (format "CTCP CLIENTINFO from %s\n" user))
    (riece-insert-change
     (if (and riece-channel-buffer-mode
	      (not (eq buffer riece-channel-buffer)))
	 (list riece-dialogue-buffer riece-others-buffer)
       riece-dialogue-buffer)
     (concat
      (riece-concat-server-name
       (format (riece-mcat "CTCP CLIENTINFO from %s (%s) to %s")
	       user
	       (riece-strip-user-at-host (riece-prefix-user-at-host prefix))
	       (riece-format-identity target-identity t)))
      "\n"))))

(defun riece-ctcp-action-format-message (message &optional global)
  (riece-with-server-buffer (riece-identity-server
			     (riece-message-speaker message))
    (concat
     (if global
	 (riece-concat-server-name
	  (concat riece-ctcp-action-prefix
		  (riece-format-identity (riece-message-target message) t) ": "
		  (riece-identity-prefix (riece-message-speaker message)) " "
		  (riece-message-text message)))
       (concat riece-ctcp-action-prefix
	       (riece-identity-prefix (riece-message-speaker message)) " "
	       (riece-message-text message)))
     "\n")))

(defun riece-handle-ctcp-action-request (prefix target string)
  (let ((buffer (if (riece-channel-p target)
		    (riece-channel-buffer (riece-make-identity
					   target riece-server-name))))
	(user (riece-prefix-nickname prefix)))
    (riece-display-message
     (riece-make-message (riece-make-identity user
					      riece-server-name)
			 (riece-make-identity target
					      riece-server-name)
			 string
			 'action
			 (riece-identity-equal-no-server
			  user riece-real-nickname)))))

(defun riece-handle-ctcp-time-request (prefix target string)
  (let* ((target-identity (riece-make-identity target riece-server-name))
	 (buffer (if (riece-channel-p target)
		     (riece-channel-buffer target-identity)))
	 (user (riece-prefix-nickname prefix))
	 (time (format-time-string "%c")))
    (riece-send-string
     (format "NOTICE %s :\1TIME %s\1\r\n" user time))
    (riece-insert-change buffer (format (riece-mcat "CTCP TIME from %s\n")
					user))
    (riece-insert-change
     (if (and riece-channel-buffer-mode
	      (not (eq buffer riece-channel-buffer)))
	 (list riece-dialogue-buffer riece-others-buffer)
       riece-dialogue-buffer)
     (concat
      (riece-concat-server-name
       (format (riece-mcat "CTCP TIME from %s (%s) to %s")
	       user
	       (riece-strip-user-at-host (riece-prefix-user-at-host prefix))
	       (riece-format-identity target-identity t)))
      "\n"))))

(defun riece-handle-ctcp-response (prefix string)
  (when (and (get 'riece-ctcp 'riece-addon-enabled) prefix string
	     (riece-prefix-nickname prefix))
    (let* ((parameters (riece-split-parameters string))
	   (targets (split-string (car parameters) ","))
	   (message (nth 1 parameters)))
      (if (string-match "\1\\([^ ]+\\)\\( .+\\)?\1" message)
	  (let ((response (downcase (match-string 1 message))))
	    (if (match-beginning 2)
		(setq message (substring (match-string 2 message) 1)))
	    (let ((hook
		   (intern (concat "riece-ctcp-" response "-response-hook")))
		  (function (intern-soft (concat "riece-handle-ctcp-"
						 response "-response")))
		  (after-hook
		   (intern (concat "riece-ctcp-after-" response
				   "-response-hook"))))
	      (unless (riece-funcall-ignore-errors
		       (symbol-name hook)
		       #'run-hook-with-args-until-success
		       hook prefix (car targets) message)
		(if function
		    (riece-funcall-ignore-errors
		     (symbol-name function)
		     function prefix (car targets) message))
		(riece-funcall-ignore-errors (symbol-name after-hook)
					     #'run-hook-with-args-until-success
					     after-hook prefix (car targets)
					     message)))
	    t)))))

(defun riece-handle-ctcp-version-response (prefix target string)
  (riece-insert-change
   (list riece-dialogue-buffer riece-others-buffer)
   (concat
    (riece-concat-server-name
     (format (riece-mcat "CTCP VERSION for %s (%s) = %s")
	     (riece-prefix-nickname prefix)
	     (riece-strip-user-at-host (riece-prefix-user-at-host prefix))
	     string))
    "\n")))

(defun riece-handle-ctcp-ping-response (prefix target string)
  (let* ((now (current-time))
	 (elapsed (+ (* 65536 (- (car now) (car riece-ctcp-ping-time)))
		     (- (nth 1 now) (nth 1 riece-ctcp-ping-time)))))
    (riece-insert-change
     (list riece-dialogue-buffer riece-others-buffer)
     (concat
      (riece-concat-server-name
       (format (riece-mcat "CTCP PING for %s (%s) = %d sec")
	       (riece-prefix-nickname prefix)
	       (riece-strip-user-at-host (riece-prefix-user-at-host prefix))
	       elapsed))
      "\n"))))

(defun riece-handle-ctcp-clientinfo-response (prefix target string)
  (riece-insert-change
   (list riece-dialogue-buffer riece-others-buffer)
   (concat
    (riece-concat-server-name
     (format (riece-mcat "CTCP CLIENTINFO for %s (%s) = %s")
	     (riece-prefix-nickname prefix)
	     (riece-strip-user-at-host (riece-prefix-user-at-host prefix))
	     string))
    "\n")))

(defun riece-handle-ctcp-time-response (prefix target string)
  (riece-insert-change
   (list riece-dialogue-buffer riece-others-buffer)
   (concat
    (riece-concat-server-name
     (format (riece-mcat "CTCP TIME for %s (%s) = %s")
	     (riece-prefix-nickname prefix)
	     (riece-strip-user-at-host (riece-prefix-user-at-host prefix))
	     string))
    "\n")))

(defun riece-command-ctcp-version (target)
  (interactive
   (list (riece-completing-read-identity
	  (riece-mcat "Channel/User: ")
	  (riece-get-identities-on-server (riece-current-server-name)))))
  (riece-send-string (format "PRIVMSG %s :\1VERSION\1\r\n"
			     (riece-identity-prefix target))))

(defun riece-command-ctcp-ping (target)
  (interactive
   (list (riece-completing-read-identity
	  (riece-mcat "Channel/User: ")
	  (riece-get-identities-on-server (riece-current-server-name)))))
  (riece-send-string (format "PRIVMSG %s :\1PING\1\r\n"
			     (riece-identity-prefix target)))
  (setq riece-ctcp-ping-time (current-time)))

(defun riece-command-ctcp-clientinfo (target)
  (interactive
   (list (riece-completing-read-identity
	  (riece-mcat "Channel/User: ")
	  (riece-get-identities-on-server (riece-current-server-name)))))
  (riece-send-string (format "PRIVMSG %s :\1CLIENTINFO\1\r\n"
			     (riece-identity-prefix target))))

(defun riece-command-ctcp-action (target action)
  (interactive
   (list (if current-prefix-arg
	     (riece-completing-read-identity
	      (riece-mcat "Channel/User: ")
	      (riece-get-identities-on-server (riece-current-server-name)))
	   riece-current-channel)
	 (let (message)
	   (beginning-of-line)
	   (setq message (buffer-substring (point)
					   (progn (end-of-line) (point))))
	   (if (equal message "")
	       (read-string (riece-mcat "Action: "))
	     (prog1 (read-from-minibuffer (riece-mcat "Action: ")
					  (cons message 0))
	       (if (> (forward-line) 0)
		   (insert "\n")))))))
  (if (equal action "")
      (error "No action"))
  (riece-send-string (format "PRIVMSG %s :\1ACTION %s\1\r\n"
			     (riece-identity-prefix target)
			     action))
  (riece-display-message
   (riece-make-message (riece-current-nickname) target action 'action t)))

(defun riece-command-ctcp-time (target)
  (interactive
   (list (riece-completing-read-identity
	  (riece-mcat "Channel/User: ")
	  (riece-get-identities-on-server (riece-current-server-name)))))
  (riece-send-string (format "PRIVMSG %s :\1TIME\1\r\n"
			     (riece-identity-prefix target))))

(defun riece-ctcp-requires ()
  (if (memq 'riece-highlight riece-addons)
      '(riece-highlight)))

(defvar riece-ctcp-dialogue-font-lock-keywords
  (list (concat "^" riece-time-prefix-regexp "\\("
		(regexp-quote riece-ctcp-action-prefix)
		".*\\)$")
	1 riece-ctcp-action-face t t))

(defun riece-ctcp-insinuate ()
  (add-hook 'riece-privmsg-hook 'riece-handle-ctcp-request)
  (add-hook 'riece-notice-hook 'riece-handle-ctcp-response)
  (if (memq 'riece-highlight riece-addons)
      (setq riece-dialogue-font-lock-keywords
	    (cons riece-ctcp-dialogue-font-lock-keywords
		  riece-dialogue-font-lock-keywords)))
  (unless (assq 'action riece-message-format-function-alist)
    (setq riece-message-format-function-alist
	  (cons (cons 'action #'riece-ctcp-action-format-message)
		riece-message-format-function-alist))))

(defun riece-ctcp-uninstall ()
  (remove-hook 'riece-privmsg-hook 'riece-handle-ctcp-request)
  (remove-hook 'riece-notice-hook 'riece-handle-ctcp-response)
  (setq riece-dialogue-font-lock-keywords
	(delq riece-ctcp-dialogue-font-lock-keywords
	      riece-dialogue-font-lock-keywords)))

(defun riece-ctcp-enable ()
  (define-key riece-dialogue-mode-map "\C-cv" 'riece-command-ctcp-version)
  (define-key riece-dialogue-mode-map "\C-cp" 'riece-command-ctcp-ping)
  (define-key riece-dialogue-mode-map "\C-ca" 'riece-command-ctcp-action)
  (define-key riece-dialogue-mode-map "\C-cc" 'riece-command-ctcp-clientinfo)
  (define-key riece-dialogue-mode-map "\C-ct" 'riece-command-ctcp-time))

(defun riece-ctcp-disable ()
  (define-key riece-dialogue-mode-map "\C-cv" nil)
  (define-key riece-dialogue-mode-map "\C-cp" nil)
  (define-key riece-dialogue-mode-map "\C-ca" nil)
  (define-key riece-dialogue-mode-map "\C-cc" nil)
  (define-key riece-dialogue-mode-map "\C-ct" nil))

(provide 'riece-ctcp)

;;; riece-ctcp.el ends here
