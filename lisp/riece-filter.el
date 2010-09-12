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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'riece-handle)
(require 'riece-misc)
(require 'riece-server)			;riece-close-server
(require 'riece-identity)
(require 'riece-debug)

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
	(riece-funcall-ignore-errors (symbol-name function)
				     function prefix number name
				     (riece-decode-coding-string string)))))

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
    (unless (riece-funcall-ignore-errors (symbol-name hook)
					 #'run-hook-with-args-until-success
					 hook prefix string)
      (if function
	  (riece-funcall-ignore-errors (symbol-name function)
				       function prefix string))
      (riece-funcall-ignore-errors (symbol-name after-hook)
				   #'run-hook-with-args-until-success
				   after-hook prefix string))))

(defsubst riece-chomp-string (string)
  (if (string-match "\r\\'" string)
      (substring string 0 (match-beginning 0))
    string))

(defun riece-filter (process input)
  (with-current-buffer (process-buffer process)
    (goto-char (point-max))
    (insert input)
    (unless riece-filter-running
      (unwind-protect
	  (progn
	    (setq riece-filter-running t)
	    (goto-char riece-read-point)
	    (beginning-of-line)
	    (while (looking-at ".*\n")	;the input line is finished
	      (save-excursion
		(if (looking-at
		     ":\\([^ ]+\\) +\\([0-5][0-9][0-9]\\) +\\([^ ]+\\)\
 +\\(.*\\)")
		    (riece-handle-numeric-reply
		     (match-string 1)		;prefix
		     (string-to-number (match-string 2)) ;number
		     (match-string 3)		;name
		     (riece-chomp-string (match-string 4))) ;reply string
		  (if (looking-at "\\(:\\([^ ]+\\) +\\)?\\([^ ]+\\) +\\(.*\\)")
		      (riece-handle-message
		       (match-string 2)	;optional prefix
		       (match-string 3)	;command
		       (riece-chomp-string (match-string 4))
					;params & trailing
		       )
		    (if riece-debug
			(message "Weird message from server: %s"
				 (buffer-substring (point) (progn
							     (end-of-line)
							     (point))))))))
	      (forward-line))
	    (unless riece-debug
	      (delete-region (point-min) (point)))
	    (setq riece-read-point (point)))
	(setq riece-filter-running nil)))))

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
	      (riece-debug (format "Connection closed: %s"
				   (substring status 0 (1- (length status)))))
	    (riece-debug (format "Connection to \"%s\" closed: %s"
				 server-name
				 (substring status 0 (1- (length status))))))
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
