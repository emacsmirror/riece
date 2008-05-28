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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'riece-globals)
(require 'riece-options)

(defun riece-encode-coding-string (string)
  (if (and (local-variable-p 'riece-coding-system (current-buffer))
	   riece-coding-system)		;should be nil on non-Mule environment
      (encode-coding-string string (if (consp riece-coding-system)
				       (cdr riece-coding-system)
				     riece-coding-system))
    string))

(defun riece-decode-coding-string (string)
  (if (and (local-variable-p 'riece-coding-system (current-buffer))
	   riece-coding-system)		;should be nil on non-Mule environment
      (riece-decode-coding-string-1 string
				    (if (consp riece-coding-system)
					(car riece-coding-system)
				      riece-coding-system))
    string))

(defun riece-decode-coding-string-1 (string coding-system)
  (let* ((decoded (decode-coding-string string coding-system))
	 (length (length decoded)))
    (put-text-property 0 length 'riece-decoded-encoded-string
		       string decoded)
    (put-text-property 0 length 'riece-decoded-coding-system
		       coding-system decoded)
    decoded))

;; The following functions are API used by handler functions.  For the
;; meantime DECODED is actually a string (with some text properties).
;; In the future, however, the implementation _should_ be changed so
;; that decoding phase is delayed until the body of handler functions.
(defun riece-decoded-coding-system (decoded)
  "Return the coding-system used for decoding DECODED."
  (get-text-property 0 'riece-decoded-coding-system decoded))

(defun riece-decoded-encoded-string (decoded)
  "Return the string before decoding."
  (get-text-property 0 'riece-decoded-encoded-string decoded))

(defalias 'riece-decoded-string 'identity)

(provide 'riece-coding)

;;; riece-coding.el ends here
