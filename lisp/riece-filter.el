;;; riece-filter.el --- process filter and sentinel
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

(require 'riece-handle)
(require 'riece-misc)
(require 'riece-server)			;riece-close-server
(require 'riece-identity)

(defun riece-handle-numeric-reply (prefix number name string)
  (let ((base-number (* (/ number 100) 100))
	function)
    (setq function (intern-soft (format "riece-handle-%03d-message" number)))
    (unless (and function
		 (symbol-function function))
      (setq function
	    (intern-soft
	     (format "riece-handle-default-%03d-message" base-number))))
    (if (and function
	     (symbol-function function))
	(condition-case error
	    (funcall function prefix number name
		     (riece-decode-coding-string string))
	  (error
	   (if riece-debug
	       (message "Error in `%S': %S" function error)))))))

(defun riece-handle-message (prefix message string)
  (if (and prefix
	   (string-match "![^\x00\x0d\x0a @]+@" prefix))
      (riece-user-set-user-at-host
       (riece-get-user (substring prefix 0 (match-beginning 0)))
       (riece-parse-user-at-host (substring prefix (1+ (match-beginning 0))))))
  (setq message (downcase message)
	string (riece-decode-coding-string string))
  (let ((function (intern-soft (concat "riece-handle-" message "-message")))
	(hook (intern (concat "riece-" message "-hook")))
	(after-hook (intern (concat "riece-after-" message "-hook"))))
    (unless (condition-case error
		(run-hook-with-args-until-success hook prefix string)
	      (error
	       (if riece-debug
		   (message "Error in `%S': %S" hook error))
	       nil))
      (if function
	  (condition-case error
	      (funcall function prefix string)
	    (error
	     (if riece-debug
		 (message "Error in `%S': %S" function error)))))
      (condition-case error
	  (run-hook-with-args-until-success after-hook prefix string)
	(error
	 (if riece-debug
	     (message "Error in `%S': %S" after-hook error)))))))

(defun riece-filter (process input)
  (save-excursion
    (set-buffer (process-buffer process))
    (goto-char riece-read-point)
    (unless riece-debug
      (delete-region (riece-line-beginning-position) (point-min))
      (setq riece-read-point (point)))
    (insert input)
    (goto-char (prog1 riece-read-point
		 (setq riece-read-point (point))))
    (beginning-of-line)
    (while (and (not (eobp))
		(looking-at ".*\r\n"))	;the input line is not finished
      (save-excursion
	(if (looking-at
	     ":\\([^ ]+\\) +\\([0-5][0-9][0-9]\\) +\\([^ ]+\\) +\\(.*\\)\r\n")
	    (riece-handle-numeric-reply
	     (match-string 1)		;prefix
	     (string-to-number (match-string 2)) ;number
	     (match-string 3)		;name
	     (match-string 4))		;reply string
	  (if (looking-at "\\(:\\([^ ]+\\) +\\)?\\([^ ]+\\) +\\(.*\\)\r\n")
	      (riece-handle-message
	       (match-string 2)	;optional prefix
	       (match-string 3)	;command
	       (match-string 4))	;params & trailing
	    (if riece-debug
		(message "Weird message from server: %s"
			 (buffer-substring (point) (progn
						     (end-of-line)
						     (point))))))))
      (forward-line))))

(eval-when-compile
  (autoload 'riece-exit "riece"))
(defun riece-sentinel (process status)
  (if riece-reconnect-with-password
      (let ((server-name
	     (with-current-buffer (process-buffer process)
	       riece-server-name)))
	(riece-close-server-process process)
	(riece-open-server
	 (if (equal server-name "")
	     riece-server
	   (riece-server-name-to-server server-name))
	 server-name))
    (let ((server-name (with-current-buffer (process-buffer process)
			 riece-server-name)))
      (if riece-debug
	  (if (equal server-name "")
	      (message "Connection closed: %s"
		       (substring status 0 (1- (length status))))
	    (message "Connection to \"%s\" closed: %s"
		     server-name (substring status 0 (1- (length status)))))
	(if (equal server-name "")
	    (message "Connection closed")
	  (message "Connection to \"%s\" closed" server-name)))
      (let ((channels riece-current-channels))
	(while channels
	  (if (and (car channels)
		   (equal (riece-identity-server (car channels))
			  server-name))
	      (riece-part-channel (car channels)))
	  (setq channels (cdr channels))))
      (riece-redisplay-buffers)
      (riece-close-server-process process)
      ;; If no server process is available, exit.
      (unless riece-server-process-alist
	(riece-exit)))))

(provide 'riece-filter)

;;; riece-filter.el ends here
