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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'riece-globals)
(require 'riece-options)

(defun riece-debug-1 (message detail)
  (message "riece-debug: %s" message)
  (with-current-buffer riece-debug-buffer
    (goto-char (point-max))
    (let ((time (format-time-string "%Y-%m-%d:%H:%M:%S")))
      (if detail
	  (insert "*** " time ": " message "\n" detail "\n")
	(insert "*** " time ": " message "\n")))))

(defun riece-debug (message &optional detail)
  "Print a one-line debug MESSAGE at the bottom of the frame.
If the optional 2nd argument DETAIL is specified, it is stored into
`riece-debug-buffer'."
  (ignore (riece-debug-1 message detail)))

(defun riece-debug-reset-standard-output ()
  "Reset `riece-temp-buffer' to be used as `standard-output'."
  (with-current-buffer riece-temp-buffer
    (buffer-disable-undo)
    (erase-buffer)))

(defmacro riece-debug-with-backtrace (&rest body)
  "Execute BODY and send a backtrace to `riece-temp-buffer'."
  `(unwind-protect
       (progn ,@body)
     (riece-debug-reset-standard-output)
     (let ((standard-output riece-temp-buffer))
       (backtrace))))

(put 'riece-debug-with-backtrace 'lisp-indent-function 0)
(put 'riece-debug-with-backtrace 'edebug-form-spec '(form body))

(defmacro riece-ignore-errors (location &rest body)
  "Execute BODY; if an error occurs, return nil.
Otherwise, return result of last FORM.
If `riece-debug' is non-nil and an error occurred, it sends a
backtrace to standard-output."
  `(condition-case error
       (if riece-debug
	   (riece-debug-with-backtrace ,@body)
	 ,@body)
     (error
      (if riece-debug
	  (with-current-buffer riece-temp-buffer
	    (goto-char (point-min))
	    (if (re-search-forward "^  signal(" nil t)
		(delete-region (point-min) (match-beginning 0)))
	    (riece-debug (format "Error in `%s': %S" ,location error)
			 (buffer-string))))
      nil)))

(put 'riece-ignore-errors 'lisp-indent-function 1)
(put 'riece-ignore-errors 'edebug-form-spec '(form body))

(defun riece-funcall-ignore-errors (location function &rest args)
  "Call FUNCTION with ARGS; if an error occurs, return nil.
Otherwise, return result of the function.
If `riece-debug' is non-nil and an error occurred, it sends a
backtrace to standard-output."
  (riece-ignore-errors location
    (apply function args)))

(provide 'riece-debug)

;;; riece-debug.el ends here
