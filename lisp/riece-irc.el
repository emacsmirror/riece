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
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(require 'riece-filter)
(require 'riece-server)

(defun riece-irc-open-server (server server-name)
  (riece-server-keyword-bind server
    (let (selective-display
	  (coding-system-for-read 'binary)
	  (coding-system-for-write 'binary)
	  process)
      (if (equal server-name "")
	  (message "Connecting to IRC server...")
	(message "Connecting to %s..." server-name))
      (condition-case error
	  (setq process
		(funcall function (riece-server-process-name server-name)
			 (concat " *IRC*" server-name)
			 host service))
	(error
	 (if (equal server-name "")
	     (message "Connecting to IRC server...failed: %S" error)
	   (message "Connecting to %s...failed: %S" server-name error))
	 (signal (car error) (cdr error))))
      (if (equal server-name "")
	  (message "Connecting to IRC server...done")
	(message "Connecting to %s...done" server-name))
      (riece-reset-process-buffer process)
      (with-current-buffer (process-buffer process)
	(setq riece-server-name server-name))
      (set-process-sentinel process 'riece-sentinel)
      (set-process-filter process 'riece-filter)
      (if (equal server-name "")
	  (message "Logging in to IRC server...")
	(message "Logging in to %s..." server-name))
      (if riece-reconnect-with-password	;password incorrect or not set.
	  (unwind-protect
	      (setq password
		    (condition-case nil
			(let (inhibit-quit)
			  (if (equal server-name "")
			      (riece-read-passwd "Password: ")
			    (riece-read-passwd (format "Password for %s: "
						       server-name))))
		      (quit
		       (if (equal server-name "")
			   (message "Password: Quit")
			 (message (format "Password for %s: Quit"
					  server-name)))
		       'quit)))
	    (setq riece-reconnect-with-password nil)))
      (if (eq password 'quit)
	  (delete-process process)
	(if password
	    (riece-process-send-string process
				       (format "PASS %s\r\n" password)))
	(riece-process-send-string process
				   (format "USER %s * * :%s\r\n"
					   (if (and username
						    (not (string-match
							  "[^\0\r\n @]"
							  username)))
					       username
					     (user-real-login-name))
					   (or realname
					       username
					       "No information given")))
	(riece-process-send-string process (format "NICK %s\r\n" nickname))
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
  (riece-process-send-string process
			     (if message
				 (format "QUIT :%s\r\n" message)
			       "QUIT\r\n")))

(provide 'riece-irc)