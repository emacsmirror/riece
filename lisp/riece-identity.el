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

(eval-when-compile (require 'riece-inlines))

(require 'riece-globals)

(defun riece-find-server-name ()
  (or riece-overriding-server-name
					;already in the server buffer
      (if (local-variable-p 'riece-server-name (current-buffer))
	  riece-server-name
	(if riece-current-channel
	    (riece-identity-server riece-current-channel)))))

(defun riece-find-server-process ()
  (let ((server-name (riece-find-server-name)))
    (if server-name
	(cdr (assoc server-name riece-server-process-alist))
      riece-server-process)))

(defmacro riece-with-server-buffer (&rest body)
  `(let ((process (riece-find-server-process)))
     (if process
	 (with-current-buffer (process-buffer process)
	   ,@body)
       (error "Server closed."))))

(defun riece-identity-prefix (identity)
  "Return the component sans its server from IDENTITY."
  (if (string-match " " identity)
      (substring identity 0 (match-beginning 0))
    identity))

(defun riece-identity-server (identity)
  "Return the server component in IDENTITY."
  (if (string-match " " identity)
      (substring identity (match-end 0))))

(defun riece-make-identity (prefix &optional server)
  "Make an identity object from PREFIX and SERVER."
  (if (riece-identity-server prefix)
      prefix
    (unless server
      (setq server (riece-find-server-name)))
    (if server
	(concat prefix " " server)
      prefix)))

(defun riece-identity-equal (ident1 ident2)
  "Return t, if IDENT1 and IDENT2 is equal."
  (and (scandinavian-equal-ignore-case
	(riece-identity-prefix ident1)
	(riece-identity-prefix ident2))
       (equal
	(riece-identity-server ident1)
	(riece-identity-server ident2))))

(defun riece-identity-equal-safe (ident1 ident2)
  "Return t, if IDENT1 and IDENT2 is equal.
The only difference with `riece-identity-equal', this function appends
server name before comparison."
  (riece-identity-equal
   (if (riece-identity-server ident1)
       ident1
     (riece-make-identity ident1))
   (if (riece-identity-server  ident2)
       ident2
     (riece-make-identity ident2))))

(defun riece-identity-member (elt list)
  "Return non-nil if an identity ELT is an element of LIST."
  (catch 'found
    (while list
      (if (and (stringp (car list))
	       (riece-identity-equal (car list) elt))
	  (throw 'found list)
	(setq list (cdr list))))))

(defun riece-identity-member-safe (elt list)
  "Return non-nil if an identity ELT is an element of LIST.
The only difference with `riece-identity-member', this function uses
`riece-identity-equal-safe' for comparison."
  (catch 'found
    (while list
      (if (and (stringp (car list))
	       (riece-identity-equal-safe (car list) elt))
	  (throw 'found list)
	(setq list (cdr list))))))

(defun riece-identity-assoc (elt alist)
  "Return non-nil if an identity ELT matches the car of an element of ALIST."
  (catch 'found
    (while alist
      (if (riece-identity-equal (car (car alist)) elt)
	  (throw 'found (car alist))
	(setq alist (cdr alist))))))

(defun riece-identity-assoc-safe (elt alist)
  "Return non-nil if an identity ELT matches the car of an element of ALIST.
The only difference with `riece-identity-assoc', this function uses
`riece-identity-equal-safe' for comparison."
  (catch 'found
    (while alist
      (if (riece-identity-equal-safe (car (car alist)) elt)
	  (throw 'found (car alist))
	(setq alist (cdr alist))))))

(defun riece-identity-assign-binding (item list binding)
  (let ((slot (riece-identity-member-safe item binding))
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

(defun riece-current-nickname ()
  "Return the current nickname."
  (riece-with-server-buffer
   (if riece-real-nickname
       (riece-make-identity riece-real-nickname))))

(provide 'riece-identity)

;;; riece-identity.el ends here
