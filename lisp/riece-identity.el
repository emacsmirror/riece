;;; riece-identity.el --- an identity object
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

;;; Code:

(require 'riece-globals)
(require 'riece-coding)
(require 'riece-server)

(defun riece-identity-prefix (identity)
  "Return the component sans its server from IDENTITY."
  (aref identity 0))

(defun riece-identity-server (identity)
  "Return the server component in IDENTITY."
  (aref identity 1))

(defun riece-make-identity (prefix server)
  "Make an identity object from PREFIX and SERVER."
  (vector prefix server))

(defun riece-identity-equal (ident1 ident2)
  "Return t, if IDENT1 and IDENT2 is equal."
  (and (riece-identity-equal-no-server
	(riece-identity-prefix ident1)
	(riece-identity-prefix ident2))
       (equal
	(riece-identity-server ident1)
	(riece-identity-server ident2))))

(defun riece-identity-canonicalize-prefix (prefix)
  "Canonicalize identity PREFIX.
This function downcases PREFIX first, then does special treatment for
Scandinavian alphabets.

RFC2812, 2.2 \"Character codes\" says:
   Because of IRC's Scandinavian origin, the characters {}|^ are
   considered to be the lower case equivalents of the characters []\~,
   respectively. This is a critical issue when determining the
   equivalence of two nicknames or channel names."
  (let* ((result (downcase prefix))
	 (length (length result))
	 (index 0))
    (while (< index length)
      (if (eq (aref result index) ?\[)
	  (aset result index ?{)
	(if (eq (aref result index) ?\])
	    (aset result index ?})
	  (if (eq (aref result index) ?\\)
	      (aset result index ?|)
	    (if (eq (aref result index) ?~)
		(aset result index ?^)))))
      (setq index (1+ index)))
    result))

(defun riece-identity-equal-no-server (prefix1 prefix2)
  "Return t, if IDENT1 and IDENT2 is equal without server."
  (equal (riece-identity-canonicalize-prefix prefix1)
	 (riece-identity-canonicalize-prefix prefix2)))

(defun riece-identity-member (elt list)
  "Return non-nil if an identity ELT is an element of LIST."
  (catch 'found
    (while list
      (if (and (vectorp (car list))	;needed because
					;riece-current-channels
					;contains nil.
	       (riece-identity-equal (car list) elt))
	  (throw 'found list)
	(setq list (cdr list))))))

(defun riece-identity-assoc (elt alist)
  "Return non-nil if an identity ELT matches the car of an element of ALIST."
  (catch 'found
    (while alist
      (if (riece-identity-equal (car (car alist)) elt)
	  (throw 'found (car alist))
	(setq alist (cdr alist))))))

(defun riece-identity-assign-binding (item list binding)
  (let ((slot (riece-identity-member item binding))
	pointer)
    (unless list			;we need at least one room
      (setq list (list nil)))
    (setq pointer list)
    (if slot
	(while (not (eq binding slot))
	  (unless (cdr pointer)
	    (setcdr pointer (list nil)))
	  (setq pointer (cdr pointer)
		binding (cdr binding)))
      (while (or (car pointer) (car binding))
	(unless (cdr pointer)
	  (setcdr pointer (list nil)))
	(setq pointer (cdr pointer)
	      binding (cdr binding))))
    (setcar pointer item)
    list))

(defmacro riece-with-identity-buffer (identity &rest body)
  `(let ((process (riece-server-process (riece-identity-server ,identity))))
     (if process
	 (with-current-buffer (process-buffer process)
	   ,@body)
       (error "Server closed"))))

(put 'riece-with-identity-buffer 'lisp-indent-function 1)

(defun riece-decode-identity (identity &optional prefix-only)
  (riece-with-identity-buffer identity
    (let ((prefix (riece-decode-coding-string
		   (riece-identity-prefix identity)))
	  (server (riece-identity-server identity)))
      (if (or prefix-only (equal server ""))
	  prefix
	(concat prefix " " server)))))

(defun riece-encode-identity (string)
  (let ((prefix (if (string-match " " string)
		    (substring string 0 (match-beginning 0))
		  string))
	(server (if (string-match " " string)
		    (substring string (match-end 0))
		  "")))
    (riece-with-server-buffer server
      (riece-make-identity (riece-encode-coding-string prefix) server))))

(defun riece-completing-read-identity (prompt channels
					      &optional predicate must-match)
  (let* ((decoded
	  (completing-read
	   prompt
	   (delq nil (mapcar (lambda (channel)
			       (list (riece-decode-identity channel)))
			     (or channels riece-current-channels)))
	   predicate must-match))
	 (encoded
	  (riece-encode-identity decoded)))
    (if (and (not (string-match "[ ,]" decoded))
	     (string-match "[ ,]" (riece-identity-prefix encoded))
	     (not (y-or-n-p (format "The encoded channel name contains illegal character \"%s\".  continue? "
				    (match-string 0 (riece-identity-prefix encoded))))))
	(error "Invalid channel name!"))
    encoded))

(provide 'riece-identity)

;;; riece-identity.el ends here
