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

(require 'riece-globals)

(defun riece-debug-reset-standard-output ()
  (save-excursion
    (set-buffer riece-temp-buffer)
    (buffer-disable-undo)
    (erase-buffer)))

(defmacro riece-debug-with-backtrace (&rest body)
  `(unwind-protect
       (progn ,@body)
     (riece-debug-reset-standard-output)
     (let ((standard-output riece-temp-buffer))
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
	  (save-excursion
	    (set-buffer riece-temp-buffer)
	    (if (re-search-forward "^  signal(" nil t)
		(delete-region (point-min) (match-beginning 0)))
	    (message "Error in `%s': %S\n%s" ,location error (buffer-string))))
      nil)))

(put 'riece-ignore-errors 'lisp-indent-function 1)
(put 'riece-ignore-errors 'edebug-form-spec '(form body))

(provide 'riece-debug)

;;; riece-debug.el ends here
