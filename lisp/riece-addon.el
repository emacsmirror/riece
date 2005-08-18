;;; riece-addon.el --- add-on management
;; Copyright (C) 1998-2004 Daiki Ueno

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

(require 'font-lock)
(require 'riece-options)
(require 'riece-compat)
(require 'riece-misc)

(defgroup riece-addon-list nil
  "Add-on listing."
  :tag "Addon list"
  :prefix "riece-addon-list-"
  :group 'riece)

(defgroup riece-addon-list-faces nil
  "Faces for riece-addon-list-mode."
  :tag "Faces"
  :prefix "riece-addon-list-"
  :group 'riece-addon-list)

(defface riece-addon-list-enabled-face
  '((((class color) (background dark))
     (:foreground "PaleTurquoise" :bold t))
    (t
     (:bold t)))
  "Face used for displaying the enabled addon."
  :group 'riece-addon-list-faces)
(defvar riece-addon-list-enabled-face 'riece-addon-list-enabled-face)

(defface riece-addon-list-disabled-face
  '((((class color) (background dark))
     (:foreground "PaleTurquoise" :italic t))
    (t
     (:italic t)))
  "Face used for displaying the disabled addon."
  :group 'riece-addon-list-faces)
(defvar riece-addon-list-disabled-face 'riece-addon-list-disabled-face)

(defface riece-addon-list-unsupported-face
  '((((class color) (background dark))
     (:foreground "PaleTurquoise"))
    (t
     ()))
  "Face used for displaying the unsupported addon."
  :group 'riece-addon-list-faces)
(defvar riece-addon-list-unsupported-face 'riece-addon-list-unsupported-face)

(defface riece-addon-list-unknown-face
  '((t
     (:foreground "red")))
  "Face used for displaying the unknown addon."
  :group 'riece-addon-list-faces)
(defvar riece-addon-list-unknown-face 'riece-addon-list-unknown-face)

(defface riece-addon-list-description-face
  '((((class color)
      (background dark))
     (:foreground "lightyellow"))
    (((class color)
      (background light))
     (:foreground "blue4"))
    (t
     ()))
  "Face used for displaying the description addon."
  :group 'riece-addon-list-faces)
(defvar riece-addon-list-description-face 'riece-addon-list-description-face)

(defcustom riece-addon-list-mark-face-alist
  '((?+ . riece-addon-list-enabled-face)
    (?- . riece-addon-list-disabled-face)
    (?! . riece-addon-list-unsupported-face)
    (?? . riece-addon-list-unknown-face))
  "An alist mapping marks on riece-addon-list-buffer to faces."
  :type 'list
  :group 'riece-addon-list)

(defcustom riece-addon-list-font-lock-keywords
  '(("^\\([-+!?] [^:]+\\): \\(.*\\)"
     (1 (cdr (assq (aref (match-string 1) 0)
		   riece-addon-list-mark-face-alist)))
     (2 riece-addon-list-description-face)))
  "Default expressions to addon in riece-addon-list-mode."
  :type '(repeat (list string))
  :group 'riece-addon-list)

(defvar riece-addon-list-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "+" 'riece-command-enable-addon)
    (define-key keymap "-" 'riece-command-disable-addon)
    (define-key keymap "n" 'next-line)
    (define-key keymap "p" 'previous-line)
    (define-key keymap " " 'scroll-up)
    (define-key keymap [delete] 'scroll-down)
    (define-key keymap "q" 'bury-buffer)
    keymap))

(defun riece-load-and-build-addon-dependencies (addons)
  (let ((load-path (cons riece-addon-directory load-path))
	dependencies)
    (while addons
      (require (car addons))		;error will be reported here
      (let* ((requires
	      (funcall (or (intern-soft
			    (concat (symbol-name (car addons)) "-requires"))
			   #'ignore)))
	     (pointer requires)
	     entry)
	;; Increment succs' pred count.
	(if (setq entry (assq (car addons) dependencies))
	    (setcar (cdr entry) (+ (length requires) (nth 1 entry)))
	  (setq dependencies (cons (list (car addons) (length requires))
				   dependencies)))
	;; Merge pred's succs.
	(while pointer
	  (if (setq entry (assq (car pointer) dependencies))
	      (setcdr (cdr entry)
		      (cons (car addons) (nthcdr 2 entry)))
	    (setq dependencies (cons (list (car pointer) 0 (car addons))
				     dependencies)))
	  (setq pointer (cdr pointer))))
      (setq addons (cdr addons)))
    dependencies))

(defun riece-resolve-addon-dependencies (addons)
  (let ((pointer addons)
	dependencies queue)
    ;; Uniquify, first.
    (while pointer
      (if (memq (car pointer) (cdr pointer))
	  (setcar pointer nil))
      (setq pointer (cdr pointer)))
    (setq dependencies (riece-load-and-build-addon-dependencies
			(delq nil addons))
	  pointer dependencies)
    ;; Sort them.
    (while pointer
      (if (zerop (nth 1 (car pointer)))
	  (setq dependencies (delq (car pointer) dependencies)
		queue (cons (car pointer) queue)))
      (setq pointer (cdr pointer)))
    (setq addons nil)
    (while queue
      (setq addons (cons (car (car queue)) addons)
	    pointer (nthcdr 2 (car queue)))
      (while pointer
	(let* ((entry (assq (car pointer) dependencies))
	       (count (1- (nth 1 entry))))
	  (if (zerop count)
	      (progn
		(setq dependencies (delq entry dependencies)
		      queue (nconc queue (list entry))))
	    (setcar (cdr entry) count)))
	(setq pointer (cdr pointer)))
      (setq queue (cdr queue)))
    (if dependencies
	(error "Circular add-on dependency found"))
    (nreverse addons)))

(defun riece-resolve-addons (addons)
  (riece-resolve-addon-dependencies
   (if (file-directory-p riece-addon-directory)
       (append addons
	       (mapcar
		(lambda (name)
		  (unless (file-directory-p
			   (expand-file-name name riece-addon-directory))
		    (intern (file-name-sans-extension name))))
		(directory-files riece-addon-directory nil "\\`[^.]" t)))
     addons)))

(defun riece-insinuate-addon (addon &optional verbose)
  (if (get addon 'riece-addon-insinuated)
      (if verbose
	  (message "Add-on %S is alread insinuated" addon))
    (funcall (intern (concat (symbol-name addon) "-insinuate")))
    (put addon 'riece-addon-insinuated t)
    (if verbose
	(message "Add-on %S is insinuated" addon))))

(defun riece-enable-addon (addon &optional verbose)
  (let ((enabled (intern-soft (concat (symbol-name addon) "-enabled"))))
    (if (null enabled)
	(if verbose
	    (message "Add-on %S doesn't support enable/disable" addon))
      (if (symbol-value enabled)
	  (if verbose
	      (message "Add-on %S is already enabled" addon))
	(funcall (intern (concat (symbol-name addon) "-enable")))
	(if verbose
	    (message "Add-on %S enabled" addon))))))

(defun riece-disable-addon (addon &optional verbose)
  (let ((enabled (intern-soft (concat (symbol-name addon) "-enabled"))))
    (if (null enabled)
	(if verbose
	    (message "Add-on %S doesn't support enable/disable" addon))
      (if (symbol-value enabled)
	  (progn
	    (funcall (intern (concat (symbol-name addon) "-disable")))
	    (if verbose
		(message "Add-on %S disabled" addon)))
	(if verbose
	    (message "Add-on %S is already enabled" addon))))))

(put 'riece-addon-list-mode 'font-lock-defaults
     '(riece-addon-list-font-lock-keywords t))

(defun riece-addon-list-mode ()
  "Major mode for displaying addon list.
All normal editing commands are turned off."
  (kill-all-local-variables)
  (buffer-disable-undo)
  (setq major-mode 'riece-addon-list-mode
	mode-name "AddOns"
	mode-line-buffer-identification
	(riece-mode-line-buffer-identification '("Riece: %12b"))
	truncate-lines t
	buffer-read-only t)
  (use-local-map riece-addon-list-mode-map)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(riece-addon-list-font-lock-keywords t))
  ;; In XEmacs, auto-initialization of font-lock is not effective
  ;; if buffer-file-name is not set.
  (font-lock-set-defaults)
  (run-hooks 'riece-addon-list-mode-hook))

(defun riece-command-list-addons ()
  (interactive)
  (set-buffer (riece-get-buffer-create "*AddOns*" 'riece-addon-list-mode))
  (riece-addon-list-mode)
  (let ((inhibit-read-only t)
	buffer-read-only
	(pointer (sort (copy-sequence riece-addons)
		       (lambda (symbol1 symbol2)
				  (string-lessp (symbol-name symbol1)
						(symbol-name symbol2)))))
	enabled description point)
    (erase-buffer)
    (riece-kill-all-overlays)
    (while pointer
      (setq enabled (intern-soft (concat (symbol-name (car pointer))
					 "-enabled"))
	    description (intern-soft (concat (symbol-name (car pointer))
					     "-description")))
      (setq point (point))
      (insert (format "%c %S: %s\n"
		      (if (not (featurep (car pointer)))
			  ??
			(if (null enabled)
			    ?!
			  (if (symbol-value enabled)
			      ?+
			    ?-)))
		      (car pointer)
		      (if description
			  (symbol-value description)
			"(no description)")))
      (put-text-property point (point) 'riece-addon (car pointer))
      (setq pointer (cdr pointer)))
    (insert "
Symbols in the leftmost column:

  +     The add-on is enabled.
  -     The add-on is disabled.
  !	The add-on doesn't support enable/disable operation.
  ?	The add-on status is unknown.
")
    (insert (substitute-command-keys "
Useful keys:

  `\\[riece-command-enable-addon]' to enable the current add-on.
  `\\[riece-command-disable-addon]' to disable the current add-on.
"))
    (goto-char (point-min))
    (pop-to-buffer (current-buffer))
    (delete-other-windows)))

(defun riece-command-enable-addon (addon)
  (interactive
   (list
    (or (if (eq major-mode 'riece-addon-list-mode)
	    (get-text-property (point) 'riece-addon))
	(intern-soft
	 (completing-read "Add-on: "
			  (mapcar (lambda (addon)
				    (list (symbol-name addon)))
				  riece-addons)
			  (lambda (pointer)
			    (let ((enabled
				   (intern-soft (concat (car pointer)
							"-enabled"))))
			      (and enabled
				   (null (symbol-value enabled)))))
			  t)))))
  (riece-enable-addon addon t)
  (let ((enabled (intern-soft (concat (symbol-name addon) "-enabled"))))
    (if (and (eq major-mode 'riece-addon-list-mode)
	     (get-text-property (point) 'riece-addon)
	     enabled (symbol-value enabled))
	(save-excursion
	  (beginning-of-line)
	  (let ((point (point))
		(inhibit-read-only t)
		buffer-read-only)
	    (delete-char 1)
	    (insert "+")
	    (put-text-property point (point) 'riece-addon addon))))))

(defun riece-command-disable-addon (addon)
  (interactive
   (list
    (or (if (eq major-mode 'riece-addon-list-mode)
	    (get-text-property (point) 'riece-addon))
	(intern-soft
	 (completing-read "Add-on: "
			  (mapcar (lambda (addon)
				    (list (symbol-name addon)))
				  riece-addons)
			  (lambda (pointer)
			    (let ((enabled
				   (intern-soft (concat (car pointer)
							"-enabled"))))
			      (and enabled
				   (symbol-value enabled))))
			  t)))))
  (riece-disable-addon addon t)
  (let ((enabled (intern-soft (concat (symbol-name addon) "-enabled"))))
    (if (and (eq major-mode 'riece-addon-list-mode)
	     (get-text-property (point) 'riece-addon)
	     enabled (null (symbol-value enabled)))
	(save-excursion
	  (beginning-of-line)
	  (let ((point (point))
		(inhibit-read-only t)
		buffer-read-only)
	    (delete-char 1)
	    (insert "-")
	    (put-text-property point (point) 'riece-addon addon))))))

(provide 'riece-addon)

;;; riece-addon.el ends here
