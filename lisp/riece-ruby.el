;;; riece-ruby.el --- interact with Ruby interpreter
;; Copyright (C) 1998-2005 Daiki Ueno

;; Author: Daiki Ueno <ueno@unixuser.org>
;; Created: 1998-09-28
;; Keywords: IRC, riece, Ruby

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

;; riece-ruby.el is a library to interact with Ruby interpreter.
;; It supports concurrent execution of Ruby programs in a single
;; session.  For example:
;; 
;; (riece-ruby-execute "sleep 30"); returns immediately
;; => "rubyserv0"
;;
;; (riece-ruby-execute "1 + 1")
;; => "rubyserv1"
;;
;; (riece-ruby-execute "\"")
;; => "rubyserv2"
;;
;; (riece-ruby-inspect "rubyserv0")
;; => ((OK nil) nil (("running")))
;;
;; (riece-ruby-inspect "rubyserv1")
;; => ((OK nil) "2" (("finished")))
;;
;; (riece-ruby-inspect "rubyserv2")
;; => ((OK nil) "(eval):1: unterminated string meets end of file" (("exited")))

;;; Code:

(require 'riece-debug)

(defgroup riece-ruby nil
  "Interact with Ruby interpreter."
  :prefix "riece-"
  :group 'riece)

(defcustom riece-ruby-command "ruby"
  "Command name for Ruby interpreter."
  :type 'string
  :group 'riece-ruby)

(defcustom riece-ruby-out-file (expand-file-name "riece-ruby.out"
						 riece-directory)
  "A file which records stdout of Ruby programs."
  :type 'string
  :group 'riece-ruby)

(defcustom riece-ruby-err-file (expand-file-name "riece-ruby.err"
						 riece-directory)
  "A file which records stderr of Ruby programs."
  :type 'string
  :group 'riece-ruby)

(defcustom riece-ruby-log-file (expand-file-name "riece-ruby.log"
						 riece-directory)
  "A file used to logging."
  :type 'string
  :group 'riece-ruby)

(defvar riece-ruby-server-program "server.rb"
  "The server program file.  If the filename is not absolute, it is
assumed that the file is in the same directory of this file.")

(defvar riece-ruby-server-program-arguments (list "-o" riece-ruby-out-file
						  "-e" riece-ruby-err-file
						  "-l" riece-ruby-log-file)
  "Command line arguments passed to `riece-ruby-server-program'.")

(defvar riece-ruby-process nil
  "Process object of Ruby interpreter.")

(defvar riece-ruby-lock nil
  "Lock for waiting server response.
Local to the process buffer.")
(defvar riece-ruby-response nil
  "The server response.
Local to the process buffer.")
(defvar riece-ruby-data nil
  "Data from server.
Local to the process buffer.")
(defvar riece-ruby-escaped-data nil
  "Escaped data from server.  This variable is cleared every time
server response arrives.
Local to the process buffer.")
(defvar riece-ruby-status-alist nil
  "Status from server.
Local to the process buffer.")

(defvar riece-ruby-output-queue-alist nil
  "An alist mapping from program name to output data.")
(defvar riece-ruby-output-handler-alist nil
  "An alist mapping from program name to output handler.
Output handlers are called every time \"# output\" line arrives.
Use `riece-ruby-set-output-handler' to set this variable.")
(defvar riece-ruby-exit-handler-alist nil
  "An alist mapping from program name to exit handler.
Exit handlers are called once when \"# exit\" line arrives.
Use `riece-ruby-set-exit-handler' to set this variable.")
(defvar riece-ruby-property-alist nil
  "An alist mapping from program name to the property list.
Use `riece-ruby-set-property' to set this variable.")

(defun riece-ruby-escape-data (data)
  (let ((index 0))
    (while (string-match "[%\r\n]+" data index)
      (setq data (replace-match
		  (mapconcat (lambda (c) (format "%%%02X" c))
			     (match-string 0 data) "")
		  nil nil data)
	    index (+ (match-end 0)
		     (* (- (match-end 0) (match-beginning 0)) 2))))
    data))

(defun riece-ruby-unescape-data (data)
  (let ((index 0))
    (while (string-match "%\\([0-9A-F][0-9A-F]\\)" data index)
      (setq data (replace-match
		  (read (concat "\"\\x" (match-string 1 data) "\""))
		  nil nil data)
	    index (- (match-end 0) 2)))
    data))

(defun riece-ruby-reset-process-buffer ()
  (with-current-buffer (process-buffer riece-ruby-process)
    (buffer-disable-undo)
    (make-local-variable 'riece-ruby-response)
    (setq riece-ruby-response nil)
    (make-local-variable 'riece-ruby-data)
    (setq riece-ruby-data nil)
    (make-local-variable 'riece-ruby-escaped-data)
    (setq riece-ruby-escaped-data nil)
    (make-local-variable 'riece-ruby-status-alist)
    (setq riece-ruby-status-alist nil)))

(defun riece-ruby-send-eval (program)
  (let* ((string (riece-ruby-escape-data program))
	 (length (- (length string) 998))
	 (index 0)
	 data)
    (while (< index length)
      (setq data (cons (substring string index (setq index (+ index 998)))
		       data)))
    (setq data (cons (substring string index) data)
	  data (nreverse data))
    (process-send-string riece-ruby-process "EVAL\r\n")
    (while data
      (process-send-string riece-ruby-process
			   (concat "D " (car data) "\r\n"))
      (setq data (cdr data)))
    (process-send-string riece-ruby-process "END\r\n")))

(defun riece-ruby-send-poll (name)
  (process-send-string riece-ruby-process
		       (concat "POLL " name "\r\n")))

(defun riece-ruby-send-exit (name)
  (process-send-string riece-ruby-process
		       (concat "EXIT " name "\r\n")))

(defun riece-ruby-filter (process input)
  (with-current-buffer (process-buffer process)
    (goto-char (point-max))
    (insert input)
    (goto-char (point-min))
    (beginning-of-line)
    (while (looking-at ".*\r\n")
      (if (looking-at "OK\\( \\(.*\\)\\)?\r")
	  (progn
	    (if riece-ruby-escaped-data
		(setq riece-ruby-data (mapconcat #'riece-ruby-unescape-data
						 riece-ruby-escaped-data "")))
	    (setq riece-ruby-escaped-data nil
		  riece-ruby-response (list 'OK (match-string 2))
		  riece-ruby-lock nil))
	(if (looking-at "ERR \\([0-9]+\\)\\( \\(.*\\)\\)?\r")
	    (progn
	      (setq riece-ruby-escaped-data nil
		    riece-ruby-response
		    (list 'ERR (string-to-number (match-string 1))
			  (match-string 3))
		    riece-ruby-lock nil))
	  (if (looking-at "D \\(.*\\)\r")
	      (setq riece-ruby-escaped-data (cons (match-string 1)
						  riece-ruby-escaped-data))
	    (if (looking-at "S \\([^ ]*\\) \\(.*\\)\r")
		(progn
		  (setq riece-ruby-status-alist (cons (cons (match-string 1)
							    (match-string 2))
						      riece-ruby-status-alist))
		  (if (member (car (car riece-ruby-status-alist))
			      '("finished" "exited"))
		      (riece-ruby-run-exit-handler
		       (cdr (car riece-ruby-status-alist)))))
	      (if (looking-at "# output \\([^ ]*\\) \\(.*\\)\r")
		  (riece-ruby-run-output-handler (match-string 1)
						 (match-string 2)
						 (current-time))
		(if (looking-at "# exit \\(.*\\)\r")
		    (riece-ruby-run-exit-handler (match-string 1))))))))
      (forward-line))
    (delete-region (point-min) (point))))

(defun riece-ruby-run-exit-handler (name)
  (let ((entry (assoc name riece-ruby-exit-handler-alist)))
    (when entry
      (setq riece-ruby-exit-handler-alist
	    (delq entry riece-ruby-exit-handler-alist))
      (riece-funcall-ignore-errors (if (symbolp (cdr entry))
				       (symbol-name (cdr entry))
				     (format "%s-exit-handler" name))
				   (cdr entry) (car entry))
      (riece-ruby-clear name))))

(defun riece-ruby-run-output-handler (name output time)
  (let ((handler-entry (assoc name riece-ruby-output-handler-alist))
	(entry (assoc name riece-ruby-output-queue-alist)))
    (if handler-entry
	(riece-funcall-ignore-errors (if (symbolp (cdr handler-entry))
					 (symbol-name (cdr handler-entry))
				       (format "%s-output-handler" name))
				     (cdr handler-entry) name output time)
      (if entry
	  (setcdr entry (cons (cons output time) (cdr entry)))
	(setq riece-ruby-output-queue-alist
	      (cons (list name (cons output time))
		    riece-ruby-output-queue-alist))))))

(defun riece-ruby-sentinel (process status)
  (kill-buffer (process-buffer process)))

(defun riece-ruby-execute (program)
  "Schedule an execution of a Ruby PROGRAM.
Return a string name assigned by the server."
  (unless (and riece-ruby-process
	       (eq (process-status riece-ruby-process) 'run))
    (let (selective-display
	  (coding-system-for-write 'binary)
	  (coding-system-for-read 'binary))
      (setq riece-ruby-process
	    (apply #'start-process "riece-ruby" (generate-new-buffer " *Ruby*")
		   riece-ruby-command
		   (expand-file-name riece-ruby-server-program
				     riece-data-directory)
		   riece-ruby-server-program-arguments))
      (riece-set-process-query-on-exit-flag riece-ruby-process nil)
      (set-process-filter riece-ruby-process #'riece-ruby-filter)
      (set-process-sentinel riece-ruby-process #'riece-ruby-sentinel)))
  (with-current-buffer (process-buffer riece-ruby-process)
    (riece-ruby-reset-process-buffer)
    (make-local-variable 'riece-ruby-lock)
    (setq riece-ruby-lock t)
    (riece-ruby-send-eval program)
    (while riece-ruby-lock
      (accept-process-output riece-ruby-process))
    (if (eq (car riece-ruby-response) 'ERR)
	(error "Couldn't execute: %S" (cdr riece-ruby-response)))
    (cdr (assoc "name" riece-ruby-status-alist))))

(defun riece-ruby-inspect (name)
  "Inspect a result of program execution distinguished by NAME.
Return a three element list.
The car is protocol response line which looks like:
  \(ERR 103 \"Not implemented\").
The cadr is data from the server, that is, the result of the program.
The caddr is status from the server."
  (with-current-buffer (process-buffer riece-ruby-process)
    (riece-ruby-reset-process-buffer)
    (make-local-variable 'riece-ruby-lock)
    (setq riece-ruby-lock t)
    (riece-ruby-send-poll name)
    (while riece-ruby-lock
      (accept-process-output riece-ruby-process))
    (list riece-ruby-response
	  riece-ruby-data
	  riece-ruby-status-alist)))

(defun riece-ruby-clear (name)
  "Clear a result of program execution distinguished by NAME.
Note that riece-ruby-clear is automatically called iff an exit-handler
is specified.  Otherwise, it should be called explicitly."
  (with-current-buffer (process-buffer riece-ruby-process)
    (riece-ruby-reset-process-buffer)
    (make-local-variable 'riece-ruby-lock)
    (setq riece-ruby-lock t)
    (riece-ruby-send-exit name)
    (while riece-ruby-lock
      (accept-process-output riece-ruby-process)))
  (let ((entry (assoc name riece-ruby-property-alist)))
    (if entry
	(delq entry riece-ruby-property-alist))))

(defun riece-ruby-set-exit-handler (name handler)
  "Set an exit-handler HANDLER for the program distinguished by NAME.
An exit-handler is called when the program is finished or exited abnormally.
An exit-handler is called with an argument same as NAME.
Note that riece-ruby-clear is automatically called iff an exit-handler
is specified.  Otherwise, it should be called explicitly."
  (let ((entry (assoc name riece-ruby-exit-handler-alist)))
    (if handler
	(progn
	  (if entry
	      (setcdr entry handler)
	    (setq riece-ruby-exit-handler-alist
		  (cons (cons name handler)
			riece-ruby-exit-handler-alist)))
	  ;;check if the program already exited
	  (riece-ruby-inspect name))
      (if entry
	  (setq riece-ruby-exit-handler-alist
		(delq entry riece-ruby-exit-handler-alist))))))

(defun riece-ruby-set-output-handler (name handler)
  "Set an output-handler HANDLER for the program distinguished by NAME.
An output-handler is called when the program sends any output by using
`output' method in the Ruby program.
An output-handler is called with three argument.  The first argument
is the same as NAME.  The second argument is the output string.  The
third argument is the timestamp of the output event."
  (let ((entry (assoc name riece-ruby-output-handler-alist))
	queue-entry pointer)
    (if handler
	(progn
	  (when (setq queue-entry (assoc name riece-ruby-output-queue-alist))
	    (setq pointer (nreverse (cdr queue-entry))
		  riece-ruby-output-queue-alist
		  (delq queue-entry riece-ruby-output-queue-alist))
	    (while pointer
	      (riece-funcall-ignore-errors (if (symbolp handler)
					       (symbol-name handler)
					     (format "%s-output-handler" name))
					   handler name (car (car pointer))
					   (cdr (car pointer)))
	      (setq pointer (cdr pointer))))
	  (if entry
	      (setcdr entry handler)
	    (setq riece-ruby-output-handler-alist
		  (cons (cons name handler)
			riece-ruby-output-handler-alist))))
      (if entry
	  (setq riece-ruby-output-handler-alist
		(delq entry riece-ruby-output-handler-alist))))))

(defun riece-ruby-set-property (name property value)
  "Set given PROPERTY/VALUE pair to the program distinguished by NAME."
  (let ((entry (assoc name riece-ruby-property-alist))
	property-entry)
    (unless entry
      (setq entry (list name)
	    riece-ruby-property-alist (cons entry riece-ruby-property-alist)))
    (if (setq property-entry (assoc property (cdr entry)))
	(setcdr property-entry value)
      (setcdr entry (cons (cons property value) (cdr entry))))))

(defun riece-ruby-property (name property)
  "Return the value of PROPERTY set to the program distinguished by NAME."
  (cdr (assoc property (cdr (assoc name riece-ruby-property-alist)))))

(defun riece-ruby-substitute-variables (program alist)
  "Substitute symbols in PROGRAM by looking up ALIST.
Return a string concatenating elements in PROGRAM."
  (setq program (copy-sequence program))
  (while alist
    (let ((pointer program))
      (while pointer
	(setq pointer (memq (car (car alist)) program))
	(if pointer
	    (setcar pointer (cdr (car alist))))))
    (setq alist (cdr alist)))
  (apply #'concat program))

(provide 'riece-ruby)

;;; riece-ruby.el ends here
