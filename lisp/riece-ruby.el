;;; riece-ruby.el --- interact with ruby interpreter
;; Copyright (C) 1998-2005 Daiki Ueno

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

;;; Commentary:

;; (riece-ruby-execute "t1" "sleep 30")
;;
;; (riece-ruby-execute "t2" "1 + 1")
;;
;; (riece-ruby-inspect "t1")
;; => ((OK nil) nil "running")
;;
;; (riece-ruby-inspect "t2")
;; => ((OK nil) "2" "finished")

;;; Code:

(defvar riece-ruby-command "ruby"
  "Command name for Ruby interpreter.")

(defvar riece-ruby-process nil)

(defvar riece-ruby-lock nil)
(defvar riece-ruby-response nil)
(defvar riece-ruby-data nil)
(defvar riece-ruby-input nil)
(defvar riece-ruby-status nil)

(defvar riece-ruby-output-handler-alist nil)
(defvar riece-ruby-exit-handler-alist nil)

(defun riece-ruby-substitute-variables (program variable value)
  (setq program (copy-sequence program))
  (let ((pointer program))
    (while pointer
      (setq pointer (memq variable program))
      (if pointer
	  (setcar pointer value)))
    program))

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

(defun riece-ruby-send-eval (name program)
  (let* ((string (riece-ruby-escape-data program))
	 (length (- (length string) 998))
	 (index 0)
	 data)
    (while (< index length)
      (setq data (cons (substring string index (setq index (+ index 998)))
		       data)))
    (setq data (cons (substring string index) data)
	  data (nreverse data))
    (save-excursion
      (set-buffer (process-buffer riece-ruby-process))
      (make-local-variable 'riece-ruby-lock)
      (setq riece-ruby-lock nil)
      (make-local-variable 'riece-ruby-response)
      (setq riece-ruby-response nil)
      (make-local-variable 'riece-ruby-data)
      (setq riece-ruby-data nil)
      (make-local-variable 'riece-ruby-input)
      (setq riece-ruby-input nil)
      (make-local-variable 'riece-ruby-status)
      (setq riece-ruby-status nil))
    (process-send-string riece-ruby-process
			 (concat "EVAL " name "\r\n"))
    (while data
      (process-send-string riece-ruby-process
			   (concat "D " (car data) "\r\n"))
      (setq data (cdr data)))
    (process-send-string riece-ruby-process "END\r\n")))
    
(defun riece-ruby-send-poll (name)
  (save-excursion
    (set-buffer (process-buffer riece-ruby-process))
    (make-local-variable 'riece-ruby-lock)
    (setq riece-ruby-lock nil)
    (make-local-variable 'riece-ruby-response)
    (setq riece-ruby-response nil)
    (make-local-variable 'riece-ruby-data)
    (setq riece-ruby-data nil)
    (make-local-variable 'riece-ruby-input)
    (setq riece-ruby-input nil)
    (make-local-variable 'riece-ruby-status)
    (setq riece-ruby-status nil))
  (process-send-string riece-ruby-process
		       (concat "POLL " name "\r\n")))

(defun riece-ruby-filter (process input)
  (save-excursion
    (set-buffer (process-buffer process))
    (goto-char (point-max))
    (insert input)
    (goto-char (process-mark process))
    (beginning-of-line)
    (while (looking-at ".*\r?\n")
      (if (looking-at "OK\\( \\(.*\\)\\)?\r")
	  (progn
	    (if riece-ruby-input
		(setq riece-ruby-data (mapconcat #'riece-ruby-unescape-data
						 riece-ruby-input "")))
	    (setq riece-ruby-input nil
		  riece-ruby-response (list 'OK (match-string 2))
		  riece-ruby-lock nil))
	(if (looking-at "ERR \\([0-9]+\\)\\( \\(.*\\)\\)?\r")
	    (progn
	      (setq riece-ruby-input nil
		    riece-ruby-response
		    (list 'ERR (string-to-number (match-string 2))
			  (match-string 3))
		    riece-ruby-lock nil))
	  (if (looking-at "D \\(.*\\)\r")
	      (setq riece-ruby-input (cons (match-string 1) riece-ruby-input))
	    (if (looking-at "S program \\(.*\\)\r")
		(setq riece-ruby-status (match-string 1))
	      (if (looking-at "# output \\(.*\\) \\(.*\\)\r")
		  (let ((entry (assoc (match-string 1)
				      riece-ruby-output-handler-alist)))
		    (if entry
			(funcall (cdr entry) (match-string 2))))
		(if (looking-at "# exit \\(.*\\)\r")
		    (let ((entry (assoc (match-string 1)
					riece-ruby-exit-handler-alist)))
		      (if entry
			  (funcall (cdr entry))))))))))
      (forward-line))
    (set-marker (process-mark process) (point-marker))))

(defun riece-ruby-sentinel (process status)
  (kill-buffer (process-buffer process)))

(defun riece-ruby-execute (name program)
  (unless (and riece-ruby-process
	       (eq (process-status riece-ruby-process) 'run))
    (setq riece-ruby-process
	  (start-process "riece-ruby" (generate-new-buffer " *Ruby*")
			 riece-ruby-command
			 (expand-file-name
			  "rubyserv.rb"
			  (file-name-directory
			   (symbol-file 'riece-ruby-execute)))))
    (set-process-filter riece-ruby-process #'riece-ruby-filter)
    (set-process-sentinel riece-ruby-process #'riece-ruby-sentinel))
  (save-excursion
    (set-buffer (process-buffer riece-ruby-process))
    (setq riece-ruby-lock t)
    (riece-ruby-send-eval name program)
    (while riece-ruby-lock
      (accept-process-output riece-ruby-process))))

(defun riece-ruby-inspect (name)
  (save-excursion
    (set-buffer (process-buffer riece-ruby-process))
    (setq riece-ruby-lock t)
    (riece-ruby-send-poll name)
    (while (null riece-ruby-response)
      (accept-process-output riece-ruby-process))
    (list riece-ruby-response
	  riece-ruby-data
	  riece-ruby-status)))

(provide 'riece-ruby)

;;; riece-ruby.el ends here
