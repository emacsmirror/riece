;;; riece-mcat.el --- message catalog
;; Copyright (C) 2007 Daiki Ueno

;; Author: Daiki Ueno <ueno@unixuser.org>

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

(require 'pp)

(defun riece-mcat (string)
  "Translate STRING in the current language environment."
  (let ((feature (if (featurep 'mule)
		     (get-language-info current-language-environment
					'riece-mcat-feature))))
    (if feature
	(progn
	  (require feature)
	  (or (cdr (assoc string
			  (symbol-value
			   (intern (concat (symbol-name feature) "-alist")))))
	      string))
      string)))

(defun riece-mcat-extract-from-form (form)
  (if (and form (listp form) (listp (cdr form)))
      (if (and (= (length form) 2)
	       (eq (car form) 'riece-mcat)
	       (stringp (car (cdr form))))
	  (cdr form)
	(delq nil (apply #'nconc
			 (mapcar #'riece-mcat-extract-from-form form))))))

(defun riece-mcat-extract (files)
  (save-excursion
    (let (message-list pointer)
      (while files
	(with-temp-buffer
	  (insert-file-contents (car files))
	  (goto-char (point-min))
	  (while (progn
		   (while (progn (skip-chars-forward " \t\n\f")
				 (looking-at ";"))
		     (forward-line 1))
		   (not (eobp)))
	    (setq message-list
		  (nconc message-list
			 (riece-mcat-extract-from-form
			  (read (current-buffer)))))))
	(setq files (cdr files)))
      (setq message-list (sort message-list #'string-lessp)
	    pointer message-list)
      (while pointer
	(if (member (car pointer) (cdr pointer))
	    (setcar pointer nil))
	(setq pointer (cdr pointer)))
      (delq nil message-list))))

(defun riece-mcat-update (files mcat-file mcat-alist-symbol)
  "Update MCAT-FILE."
  (let ((pp-escape-newlines t)
	alist)
    (with-current-buffer (find-file-noselect mcat-file)
      (goto-char (point-min))
      (if (re-search-forward (concat "^\\s-*(\\(defvar\\|defconst\\)\\s-+"
				     (regexp-quote (symbol-name
						    mcat-alist-symbol)))
			     nil t)
	  (progn
	    (goto-char (match-beginning 0))
	    (save-excursion
	      (eval (read (current-buffer))))
	    (delete-region (point) (progn (forward-sexp) (point))))
	(set mcat-alist-symbol nil))
      (setq alist (mapcar (lambda (message)
			    (or (assoc message
				       (symbol-value mcat-alist-symbol))
				(list message)))
			  (riece-mcat-extract files)))
      (insert "(defconst " (symbol-name mcat-alist-symbol) "\n  '(")
      (while alist
	(insert "(" (pp-to-string (car (car alist))) " . "
		(pp-to-string (cdr (car alist))) ")")
	(if (cdr alist)
	    (insert "\n    "))
	(setq alist (cdr alist)))
      (insert "))")
      (save-buffer))))

(defconst riece-mcat-description "Translate messages.")

(defun riece-mcat-insinuate ()
  (set-language-info "Japanese" 'riece-mcat-feature 'riece-mcat-japanese))

(defun riece-mcat-uninstall ()
  (set-language-info "Japanese" 'riece-mcat-feature nil))

(provide 'riece-mcat)

;;; riece-mcat.el ends here
