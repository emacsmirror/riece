;;; riece-signal.el --- "signal-slot" abstraction for routing display events
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

;;; Commentary:

;;; This module implements Qt like "signal-slot" abstraction for
;;; routing display events.

;;; Code:

(defvar riece-signal-slot-obarray
  (make-vector 31 0))

(defun riece-make-slot (function &optional filter handback)
  "Make an instance of slot object.
Arguments are corresponding to callback function, filter function, and
a handback object, respectively.
This function is for internal use only."
  (vector function filter handback))

(defun riece-slot-function (slot)
  "Return the callback function of SLOT.
This function is for internal use only."
  (aref slot 0))

(defun riece-slot-filter (slot)
  "Return the filter function of SLOT.
This function is for internal use only."
  (aref slot 1))

(defun riece-slot-handback (slot)
  "Return the handback object of SLOT.
This function is for internal use only."
  (aref slot 2))

(defun riece-make-signal (name args)
  "Make an instance of signal object.
The 1st arguments is the name of the signal and the rest of arguments
are the data of the signal.
This function is for internal use only."
  (vector name args))

(defun riece-signal-name (signal)
  "Return the name of SIGNAL."
  (aref signal 0))

(defun riece-signal-args (signal)
  "Return the data of SIGNAL."
  (aref signal 1))

(defun riece-connect-signal (signal-name function &optional filter handback)
  "Add SLOT as a listener of a signal identified by SIGNAL-NAME."
  (let ((symbol (intern (symbol-name signal-name) riece-signal-slot-obarray)))
    (set symbol (cons (riece-make-slot function filter handback)
		      (if (boundp symbol)
			  (symbol-value symbol))))))

(defun riece-emit-signal (signal-name &rest args)
  "Emit SIGNAL."
  (let ((symbol (intern-soft (symbol-name signal-name)
			     riece-signal-slot-obarray))
	signal
	slots)
    (when symbol
      (setq signal (riece-make-signal signal-name args)
	    slots (symbol-value symbol))
      (while slots
	(condition-case error
	    (if (or (null (riece-slot-filter (car slots)))
		    (condition-case error
			(funcall (riece-slot-filter (car slots)) signal)
		      (error
		       (if riece-debug
			   (message
			    "Error occurred in signal filter for \"%S\": %S"
			    signal-name error)))
		      nil))
		(funcall (riece-slot-function (car slots))
			 signal (riece-slot-handback (car slots))))
	  (error
	   (if riece-debug
	       (message "Error occurred in slot function for \"%S\": %S"
			signal-name error))))
	(setq slots (cdr slots))))))

(provide 'riece-signal)

;;; riece-signal.el ends here
