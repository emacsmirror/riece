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
  "Coding system."
  :tag "Coding"
  :prefix "riece-"
  :group 'riece)

(defcustom riece-default-coding-system
  (if (featurep 'mule)
      (cons 'ctext 'iso-2022-jp-2))
  "Coding system for process I/O.
The value is a coding system, or a cons cell (DECODING . ENCODING)
specifying the coding systems for decoding and encoding respectively."
  :type '(choice (symbol :tag "Coding system")
		 (cons (symbol :tag "Input coding system")
		       (symbol :tag "Output coding system"))
		 (const nil :tag "No conversion"))
  :group 'riece-coding)

(defcustom riece-coding-system-alist nil
  "An alist mapping from identities to coding-systems."
  :type '(repeat (cons (string :tag "Identity")
		       (symbol :tag "Coding system")))
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
      (riece-decode-coding-string-1 string
				    (if (consp riece-coding-system)
					(car riece-coding-system)
				      riece-coding-system))
    string))

(defun riece-decode-coding-string-1 (string coding-system)
  (let* ((decoded (decode-coding-string string coding-system))
	 (length (length decoded)))
    (put-text-property 0 length 'riece-coding-encoded-string
		       string decoded)
    (put-text-property 0 length 'riece-coding-decoded-coding-system
		       coding-system decoded)
    decoded))

(defun riece-coding-system-for-identity (identity)
  (let ((alist riece-coding-system-alist)
	matcher)
    (catch 'found
      (while alist
	(setq matcher (riece-parse-identity (car (car alist))))
	(if (and (equal (riece-identity-server matcher)
			(riece-identity-server identity))
		 (equal (riece-identity-prefix matcher)
			(riece-identity-prefix identity)))
	    (throw 'found (cdr (car alist))))
	(setq alist (cdr alist))))))

;; The following functions are API used by handler functions.  For the
;; meantime DECODED is actually a string (with some text properties).
;; In the future, however, the implementation _should_ be changed so
;; that decoding phase is delayed until the body of handler functions.
(defun riece-decoded-coding-system (decoded)
  "Return the coding-system used for decoding DECODED."
  (get-text-property 0 'riece-coding-decoded-coding-system decoded))

(defun riece-encoded-string (decoded)
  "Return the string before decoding."
  (get-text-property 0 'riece-coding-encoded-string decoded))

(defalias 'riece-decoded-string 'identity)

(defun riece-decoded-string-for-identity (decoded identity)
  "Return the string decoded for IDENTITY."
  (let ((coding-system (riece-coding-system-for-identity identity)))
    (if (and coding-system
	     (not (eq (riece-decoded-coding-system string)
		      coding-system)))
	(riece-decode-coding-string-1 (riece-encoded-string decoded)
				      coding-system)
      decoded)))

(provide 'riece-coding)

;;; riece-coding.el ends here
