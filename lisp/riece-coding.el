;;; riece-coding.el --- converting string with coding system
;; Copyright (C) 1998-2003 Daiki Ueno

;; Author: Daiki Ueno <ueno@unixuser.org>
;; Created: 1998-09-28
;; Keywords: IRC, riece, coding-system, MULE

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

(defgroup riece-coding nil
  "Coding system"
  :tag "Coding"
  :prefix "riece-"
  :group 'riece)
  
(defcustom riece-default-coding-system
  (if (and (or (featurep 'mule)
	       (featurep 'file-coding)))
      (cons 'ctext 'iso-2022-jp-2))
  "Coding system for process I/O.
The value is a coding system, or a cons cell (DECODING . ENCODING)
specifying the coding systems for decoding and encoding respectively."
  :type '(choice (symbol :tag "Coding system")
		 (cons (symbol :tag "Input coding system")
		       (symbol :tag "Output coding system"))
		 (const nil :tag "No conversion"))
  :group 'riece-coding)

(defun riece-encode-coding-string (string)
  (if (and (local-variable-p 'riece-coding-system (current-buffer))
	   riece-coding-system)		;should be nil on non-Mule environment
      (if (consp riece-coding-system)
	  (encode-coding-string string (cdr riece-coding-system))
	(encode-coding-string string riece-coding-system))
    string))

(defun riece-decode-coding-string (string)
  (if (and (local-variable-p 'riece-coding-system (current-buffer))
	   riece-coding-system)		;should be nil on non-Mule environment
      (if (consp riece-coding-system)
	  (decode-coding-string string (car riece-coding-system))
	(decode-coding-string string riece-coding-system))
    string))

(provide 'riece-coding)

;;; riece-coding.el ends here
