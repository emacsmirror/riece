;;; riece-debug.el --- debug support
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

(defvar riece-debug-standard-output
  (make-string 4096 ?\x0))

(defvar riece-debug-standard-output-index 0)

(defun riece-debug-standard-output (character)
  (let ((length (length riece-debug-standard-output)))
    (if (= riece-debug-standard-output-index length)
	(setq riece-debug-standard-output
	      (concat riece-debug-standard-output
		      (make-string length ?\x0))))
    (aset riece-debug-standard-output
	  riece-debug-standard-output-index
	  character)
    (setq riece-debug-standard-output-index
	  (1+ riece-debug-standard-output-index))))

(defmacro riece-debug-with-backtrace (&rest body)
  `(unwind-protect
       (progn ,@body)
     (setq riece-debug-standard-output-index 0)
     (let ((standard-output #'riece-debug-standard-output))
       (backtrace))))

(put 'riece-debug-with-backtrace 'lisp-indent-function 0)
(put 'riece-debug-with-backtrace 'edebug-form-spec '(form body))

(defmacro riece-ignore-errors (location &rest body)
  `(condition-case error
       (if riece-debug
	   (riece-debug-with-backtrace ,@body)
	 ,@body)
     (error
      (if riece-debug
	  (let ((backtrace (substring riece-debug-standard-output
				      0 riece-debug-standard-output-index)))
	    (if (string-match "^  signal(" backtrace)
		(setq backtrace (substring backtrace 0 (match-beginning 0))))
	    (message "Error in `%s': %S\n%s" ,location error backtrace)))
      nil)))

(put 'riece-ignore-errors 'lisp-indent-function 1)
(put 'riece-ignore-errors 'edebug-form-spec '(form body))

(provide 'riece-debug)

;;; riece-debug.el ends here
