;;; riece-inlines.el --- inline functions
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

(defsubst string-equal-ignore-case (s1 s2)
  (string-equal (upcase s1) (upcase s2)))

(defsubst string-list-member-ignore-case (thing list)
  (catch 'found
    (while list
      (if (and (stringp (car list))
	       (string-equal-ignore-case (car list) thing))
	  (throw 'found list)
	(setq list (cdr list))))))

(defsubst string-list-delete-ignore-case (thing list)
  (let ((pointer (string-list-member-ignore-case thing list)))
    (if pointer
	(delq (car pointer) list)
      list)))

(defsubst string-list-delete (thing list)
  (let ((pointer (member thing list)))
    (if pointer
	(delq (car pointer) list)
      list)))

(defsubst string-list-modify-ignore-case (modifiers list)
  (while modifiers
    (let ((pointer (string-list-member-ignore-case
		    (car (car modifiers)) list)))
      (if pointer
	  (setcar pointer (cdr (car modifiers))))
      (setq modifiers (cdr modifiers)))))

(defsubst string-assoc-ignore-case (key list)
  (catch 'found
    (while list
      (if (and (car-safe (car list))
	       (string-equal-ignore-case key (car (car list))))
	  (throw 'found (car list))
	(setq list (cdr list))))))

(provide 'riece-inlines)

;;; riece-inlines.el ends here
