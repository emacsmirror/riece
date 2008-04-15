;;; riece-irc.el --- IRC protocol
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

;;; Code:

(require 'riece-filter)
(require 'riece-server)
(require 'riece-mcat)

(defun riece-irc-open-server (server server-name)
  (riece-server-keyword-bind server
    (let (selective-display
	  (coding-system-for-read 'binary)
	  (coding-system-for-write 'binary)
	  process)
      (if (equal server-name "")
	  (message (riece-mcat "Connecting to IRC server..."))
	(message (riece-mcat "Connecting to %s...") server-name))
      (condition-case error
	  (setq process
		(funcall function (riece-server-process-name server-name)
			 (concat " *IRC*" server-name)
			 host service))
	(error
	 (if (equal server-name "")
	     (message (riece-mcat "Connecting to IRC server...failed: %S")
		      error)
	   (message (riece-mcat "Connecting to %s...failed: %S") server-name
		    error))
	 (signal (car error) (cdr error))))
      (if (equal server-name "")
	  (message (riece-mcat "Connecting to IRC server...done"))
	(message (riece-mcat "Connecting to %s...done") server-name))
      (riece-reset-process-buffer process)
      (with-current-buffer (process-buffer process)
	(setq riece-server-name server-name))
      (set-process-sentinel process 'riece-sentinel)
      (set-process-filter process 'riece-filter)
      (if (equal server-name "")
	  (message (riece-mcat "Logging in to IRC server..."))
	(message (riece-mcat "Logging in to %s...") server-name))
      (if riece-reconnect-with-password	;password incorrect or not set.
	  (unwind-protect
	      (setq password
		    (condition-case nil
			(let (inhibit-quit)
			  (if (equal server-name "")
			      (riece-read-passwd (riece-mcat "Password: "))
			    (riece-read-passwd
			     (format (riece-mcat "Password for %s: ")
				     server-name))))
		      (quit
		       (if (equal server-name "")
			   (message (riece-mcat "Password: Quit"))
			 (message (riece-mcat "Password for %s: Quit")
				  server-name))
		       'quit)))
	    (setq riece-reconnect-with-password nil)))
      (if (eq password 'quit)
	  (delete-process process)
	(if password
	    (riece-process-send-string process
				       (format "PASS %s\r\n" password)))
	(riece-process-send-string process (format "NICK %s\r\n" nickname))
	(unless realname
	  (setq realname (riece-mcat "No information given")))
	(if coding
	    (setq realname (encode-coding-string realname
						 (if (consp coding)
						     (cdr coding)
						   coding))))
	(riece-process-send-string process
				   (format "USER %s * * :%s\r\n"
					   (or username
					       (user-real-login-name))
					   realname))
	(with-current-buffer (process-buffer process)
	  (setq riece-last-nickname riece-real-nickname
		riece-nick-accepted 'sent
		riece-coding-system coding))
	process))))

(defun riece-irc-quit-server-process (process &optional message)
  (if riece-quit-timeout
      (riece-run-at-time riece-quit-timeout nil
			 (lambda (process)
			   (if (rassq process riece-server-process-alist)
			       (delete-process process)))
			 process))
  (let ((server-name (with-current-buffer (process-buffer process)
		       riece-server-name)))
    (if (equal server-name "")
	(message (riece-mcat "Sending QUIT..."))
      (message (riece-mcat "Sending QUIT to \"%s\"...") server-name))
    (riece-process-send-string process
			       (if message
				   (format "QUIT :%s\r\n" message)
				 "QUIT\r\n"))
    (if (equal server-name "")
	(message (riece-mcat "Sending QUIT...done"))
      (message (riece-mcat "Sending QUIT to \"%s\"...done") server-name))))

(provide 'riece-irc)

;;; riece-irc.el ends here
