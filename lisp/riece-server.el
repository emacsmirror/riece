;;; riece-server.el --- functions to open and close servers
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
(require 'riece-globals)		;for server local variables.
(require 'riece-misc)			;riece-process-send-string, etc.
(require 'riece-coding)			;riece-default-coding-system
(require 'riece-identity)
(require 'riece-display)

(eval-and-compile
  (defvar riece-server-keyword-map
    '((:host)
      (:service 6667)
      (:nickname riece-nickname)
      (:username riece-username)
      (:password)
      (:function #'open-network-stream)
      (:coding-system riece-default-coding-system))
    "Mapping from keywords to default values.
All keywords that can be used must be listed here."))

(defmacro riece-server-keyword-bind (plist &rest body)
  "Return a `let' form that binds all variables in PLIST.
After this is done, BODY will be executed in the scope
of the `let' form.

The variables bound and their default values are described by
the `riece-server-keyword-map' variable."
  `(let ,(mapcar
	  (lambda (keyword)
	    (list (intern (substring (symbol-name (car keyword)) 1))
		  (if (cadr keyword)
		      `(or (plist-get ,plist ',(car keyword))
			   ,(cadr keyword))
		    `(plist-get ,plist ',(car keyword)))))
	  riece-server-keyword-map)
     ,@body))

(put 'riece-server-keyword-bind 'lisp-indent-function 1)
(put 'riece-server-keyword-bind 'edebug-form-spec '(form body))

(defun riece-start-server (server &optional server-name)
  "Open network stream to remote irc server.
If optional argument CONFIRM is non-nil, ask the host that the server
is running on."
  (if server-name
      (message "Connecting to IRC server on %s..." server-name)
    (message "Connecting to IRC server..."))
  (prog1 (riece-open-server server server-name)
    (if server-name
	(message "Connecting to IRC server on %s...done" server-name)
      (message "Connecting to IRC server...done"))))

(defun riece-clear-system ()
  (while riece-buffer-list
    (if (and (get-buffer (car riece-buffer-list))
	     (buffer-live-p (car riece-buffer-list)))
	(funcall riece-buffer-dispose-function (car riece-buffer-list)))
    (setq riece-buffer-list (cdr riece-buffer-list)))
  (setq riece-channel-buffer-alist nil
	riece-user-list-buffer-alist nil
	riece-current-channels nil
	riece-current-channel nil
	riece-channel-indicator "None"
	riece-channel-list-indicator "No channel")
  (delete-other-windows))

(defun riece-server-parse-string (string)
  "Convert a STRING set as `riece-server' and return a property list."
  (when (or (string-match "^\\[\\([^]]+\\)\\]:?\\([0-9]*\\)" string)
	    (string-match "^\\([^:]+\\):?\\([0-9]*\\)" string))
    (let ((host (match-string 1 string))
	  (service (match-string 2 string))
	  (password (substring string (match-end 0)))
	  plist)
      (setq plist (cons `(:host ,host) plist))
      (unless (equal service "")
	(setq plist (cons `(:service ,(string-to-int service)) plist)))
      (unless (equal password "")
	(setq plist (cons `(:password ,(substring password 1)) plist)))
      (apply #'nconc plist))))

(defun riece-server-name-to-server (server-name)
  (let ((entry (assoc server-name riece-server-alist)))
    (if entry
	(unless (listp (cdr entry))
	  (setcdr entry (riece-server-parse-string (cdr entry))))
      (setq entry (cons server-name (riece-server-parse-string server-name))
	    riece-server-alist (cons entry riece-server-alist)
	    riece-save-variables-are-dirty t))
    (cdr entry)))

(defun riece-open-server (server server-name)
  "Open chat server on HOST.
If HOST is nil, use value of environment variable \"IRCSERVER\".
If optional argument SERVICE is non-nil, open by the service name."
  (riece-server-keyword-bind server
    (let* (selective-display
	   (coding-system-for-read 'binary)
	   (coding-system-for-write 'binary)
	   (process
	    (funcall function "IRC" (if server-name
					(format " *IRC*%s" server-name)
				      " *IRC*")
		     host service)))
      (riece-reset-process-buffer process)
      (set-process-sentinel process 'riece-sentinel)
      (set-process-filter process 'riece-filter)
      (if (or password
	      riece-reconnect-with-password)
	  (riece-process-send-string process
				     (format "PASS %s\r\n"
					     (or password
						 (riece-read-passwd
						  "Password: ")))))
      (setq riece-reconnect-with-password nil)
      (riece-process-send-string process
				 (format "USER %s * * :%s\r\n"
					 (user-real-login-name)
					 (or username
					     "No information given")))
      (riece-process-send-string process (format "NICK %s\r\n" nickname))
      (with-current-buffer (process-buffer process)
	(setq riece-last-nickname riece-real-nickname
	      riece-nick-accepted 'sent
	      riece-coding-system coding-system))
      process)))

(defun riece-reset-process-buffer (process)
  (save-excursion
    (set-buffer (process-buffer process))
    (if (fboundp 'set-buffer-multibyte)
	(set-buffer-multibyte nil))
    (kill-all-local-variables)
    (make-local-variable 'riece-real-nickname)
    (make-local-variable 'riece-last-nickname)
    (make-local-variable 'riece-nick-accepted)
    (make-local-variable 'riece-real-server-name)
    (make-local-variable 'riece-real-userhost)
    (make-local-variable 'riece-user-at-host)
    (make-local-variable 'riece-user-at-host-type)
    (make-local-variable 'riece-supported-user-modes)
    (make-local-variable 'riece-supported-channel-modes)
    (make-local-variable 'riece-channel-filter)
    (make-local-variable 'riece-server-name)
    (make-local-variable 'riece-read-point)
    (setq riece-read-point (point-min))
    (make-local-variable 'riece-obarray)
    (setq riece-obarray (make-vector riece-obarray-size 0))
    (make-local-variable 'riece-coding-system)
    (buffer-disable-undo)
    (erase-buffer)))

(defun riece-close-server-process (process &optional quit-message)
  (if (eq 'riece-filter (process-filter process))
      (set-process-filter process nil))
  (if (eq 'riece-sentinel (process-sentinel process))
      (set-process-sentinel process nil))
  (when (memq (process-status process) '(open run))
    (riece-process-send-string process
			       (if quit-message
				   (format "QUIT :%s\r\n" quit-message)
				 "QUIT\r\n"))
    (delete-process process)
    (unless riece-debug
      (kill-buffer (process-buffer process)))))

(eval-when-compile
  (autoload 'riece-exit "riece"))
(defun riece-close-server (server-name &optional quit-message)
  ;; Remove channels which belong to the server.
  (let ((riece-overriding-server-name server-name)
	(channels riece-current-channels))
    (while channels
      (if (and (car channels)
	       (equal (riece-identity-server (car channels))
		      server-name))
	  (riece-part-channel (car channels)))
      (setq channels (cdr channels)))
    (riece-redisplay-buffers))
  ;; Close now.
  (let (process)
    (if server-name
	(let ((entry (assoc server-name riece-server-process-alist)))
	  (setq process (cdr entry)
		riece-server-process-alist
		(delq entry riece-server-process-alist)))
      (setq process riece-server-process
	    riece-server-process nil))
    (riece-close-server-process process quit-message)
    ;; If no server process is available, exit.
    (if (and (null riece-server-process)
	     (null riece-server-process-alist))
	(riece-exit))))

(defun riece-close-all-server (&optional quit-message)
  (let ((process-list
	 (delq nil (cons riece-server-process
			 (mapcar #'cdr riece-server-process-alist)))))
    (while process-list
      (riece-close-server-process (car process-list) quit-message)
      (setq process-list (cdr process-list)))
    (setq riece-server-process nil
	  riece-server-process-alist nil)
    (riece-exit)))

(defun riece-server-opened (&optional server-name)
  (let ((processes
	 (delq nil
	       (if server-name
		   (cdr (assoc server-name riece-server-process-alist))
		 (cons riece-server-process
		       (mapcar #'cdr riece-server-process-alist))))))
    (catch 'found
      (while processes
	(if (memq (process-status (car processes)) '(open run))
	    (throw 'found t))
	(setq processes (cdr processes))))))

(provide 'riece-server)

;;; riece-server.el ends here
