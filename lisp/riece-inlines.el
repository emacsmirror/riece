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

;;; Commentary:

;; RFC2812, 2.2 "Character codes" says:
;;    Because of IRC's Scandinavian origin, the characters {}|^ are
;;    considered to be the lower case equivalents of the characters []\~,
;;    respectively. This is a critical issue when determining the
;;    equivalence of two nicknames or channel names.

;;; Code:

(defsubst scandinavian-downcase (string)
  (let* ((result (downcase string))
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

(defsubst scandinavian-equal-ignore-case (s1 s2)
  (string-equal (scandinavian-downcase s1) (scandinavian-downcase s2)))

(defsubst scandinavian-member-ignore-case (thing list)
  (catch 'found
    (while list
      (if (and (stringp (car list))
	       (scandinavian-equal-ignore-case (car list) thing))
	  (throw 'found list)
	(setq list (cdr list))))))

(provide 'riece-inlines)

;;; riece-inlines.el ends here
