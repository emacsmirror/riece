;;; riece-alias.el --- define aliases of names
;; Copyright (C) 1998-2003 Daiki Ueno

;; Author: Daiki Ueno <ueno@unixuser.org>
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

;; This add-on allows you to define aliases for IRC names.

;; To use, add the following line to your ~/.riece/init.el:
;; (add-to-list 'riece-addons 'riece-alias)

;; For example, if you want to define an alias `#l' for `#Liece', you
;; can customize riece-alias-alist as follows:
;; (setq riece-alias-alist '(("#Liece" . "#l")))

;;; Code:

(require 'riece-identity)

(defgroup riece-alias nil
  "Define aliases of names"
  :prefix "riece-"
  :group 'riece)

(defcustom riece-alias-percent-hack-mask "*.jp"
  "The mask of local IRC network"
  :type 'string
  :group 'riece-alias)

(defcustom riece-alias-enable-percent-hack t
  "If non-nil, the target mask is abbreviated with `%'."
  :type 'boolean
  :group 'riece-alias)

(defcustom riece-alias-alist nil
  "An alist mapping aliases to names."
  :type 'list
  :group 'riece-alias)

(defun riece-alias-abbrev-percent-hack (string)
  (if (string-match (concat "^#\\([^ ]+\\):"
			    (regexp-quote riece-alias-percent-hack-mask)
			    "\\( .+\\|$\\)")
		    string)
      (replace-match "%\\1\\2" nil nil string)
    string))

(defun riece-alias-expand-percent-hack (string)
  (if (string-match "^%\\([^ ]+\\)\\( .+\\|$\\)" string)
      (replace-match (concat "#\\1:" riece-alias-percent-hack-mask "\\2")
		     nil nil string)
    string))

(defun riece-alias-abbrev-identity-string (string)
  (if riece-alias-enable-percent-hack
      (setq string (riece-alias-abbrev-percent-hack string)))
  (let ((alist riece-alias-alist))
    (catch 'done
      (while alist
	(if (equal (car (car alist)) string)
	    (throw 'done (cdr (car alist))))
	(setq alist (cdr alist)))
      string)))

(defun riece-alias-expand-identity-string (string)
  (if riece-alias-enable-percent-hack
      (setq string (riece-alias-expand-percent-hack string)))
  (let ((alist riece-alias-alist))
    (catch 'done
      (while alist
	(if (equal (cdr (car alist)) string)
	    (throw 'done (car (car alist))))
	(setq alist (cdr alist)))
      string)))

(defun riece-alias-insinuate ()
  (setq riece-abbrev-identity-string-function
	#'riece-alias-abbrev-identity-string
	riece-expand-identity-string-function
	#'riece-alias-expand-identity-string))

(provide 'riece-alias)

;;; riece-alias.el ends here
