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

;;; Code:

(defvar riece-ruby-command "ruby"
  "Command name for Ruby interpreter.")

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

(defun riece-ruby-start ()			
  (start-process "riece-ruby"
		 (generate-new-buffer " *Ruby*")
		 riece-ruby-command
		 (expand-file-name "rubyserv.rb"
				   (file-name-directory
				    (symbol-file 'riece-ruby-start)))))

(defun riece-ruby-execute (process program &optional callback)
  (set-process-filter process #'riece-ruby-filter)
  (set-process-sentinel process #'riece-ruby-sentinel)
  (with-current-buffer (process-buffer process)
    (make-local-variable 'riece-ruby-callback)
    (setq riece-ruby-callback callback)
    (make-local-variable 'riece-ruby-data)
    (setq riece-ruby-data nil))
  (process-send-string process
		       (concat "EVAL " (riece-ruby-escape-data program)
			       "\n")))

(defun riece-ruby-filter (process input)
  (save-excursion
    (set-buffer (process-buffer process))
    (goto-char (point-max))
    (insert input)
    (goto-char (process-mark process))
    (beginning-of-line)
    (while (looking-at ".*\r?\n")
      (if (looking-at "OK\\( \\(.*\\)\\)?")
	  (progn
	    (funcall riece-ruby-callback
		     (list 'OK
			   (match-string 2)
			   (riece-ruby-unescape-data
			    (apply #'concat (nreverse riece-ruby-data)))))
	    (setq riece-ruby-data nil))
	(if (looking-at "ERR \\([0-9]+\\)\\( \\(.*\\)\\)?")
	    (progn
	      (funcall riece-ruby-callback
		       (list 'ERR
			     (number-to-string (match-string 1))
			     (match-string 3)
			     (riece-ruby-unescape-data
			      (apply #'concat (nreverse riece-ruby-data)))))
	      (setq riece-ruby-data nil))
	  (if (looking-at "D \\(.*\\)")
	      (setq riece-ruby-data (cons (match-string 1) riece-ruby-data)))))
      (forward-line))))

(defun riece-ruby-sentinel (process status)
  (kill-buffer (process-buffer process)))

(provide 'riece-ruby)

;;; riece-ruby.el ends here
