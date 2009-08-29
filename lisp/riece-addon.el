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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'font-lock)
(require 'riece-options)
(require 'riece-compat)
(require 'riece-misc)
(require 'riece-addon-modules)
(require 'riece-mcat)

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
     ()))
  "Face used for displaying the disabled addon."
  :group 'riece-addon-list-faces)
(defvar riece-addon-list-disabled-face 'riece-addon-list-disabled-face)

(defface riece-addon-list-uninstalled-face
  '((t
     (:italic t)))
  "Face used for displaying the uninstalled addon."
  :group 'riece-addon-list-faces)
(defvar riece-addon-list-uninstalled-face 'riece-addon-list-uninstalled-face)

(defface riece-addon-list-unloaded-face
  '((t
     (:italic t :inverse-video t)))
  "Face used for displaying the unloaded addon."
  :group 'riece-addon-list-faces)
(defvar riece-addon-list-unloaded-face 'riece-addon-list-unloaded-face)

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
    (?? . riece-addon-list-uninstalled-face)
    (?  . riece-addon-list-unloaded-face))
  "An alist mapping marks on riece-addon-list-buffer to faces."
  :type '(repeat (cons character symbol))
  :group 'riece-addon-list)

(defcustom riece-addon-list-font-lock-keywords
  '(("^\\([-+? ] \\S-+\\)\\s-+\\(.*\\)"
     (1 (cdr (assq (aref (match-string 1) 0)
		   riece-addon-list-mark-face-alist)))
     (2 riece-addon-list-description-face)))
  "Default expressions to addon in riece-addon-list-mode."
  :type '(repeat (list string))
  :group 'riece-addon-list)

(eval-when-compile
  (autoload 'riece-command-save-variables "riece-commands"))

(defvar riece-addon-list-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "+" 'riece-command-enable-addon)
    (define-key keymap "-" 'riece-command-disable-addon)
    (define-key keymap "i" 'riece-command-insinuate-addon)
    (define-key keymap "u" 'riece-command-uninstall-addon)
    (define-key keymap "U" 'riece-command-unload-addon)
    (define-key keymap "n" 'next-line)
    (define-key keymap "p" 'previous-line)
    (define-key keymap " " 'scroll-up)
    (define-key keymap [delete] 'scroll-down)
    (define-key keymap "q" 'bury-buffer)
    (define-key keymap "s" 'riece-command-save-variables)
    keymap))

(defun riece-load-and-build-addon-dependencies (addons)
  (let ((load-path (cons riece-addon-directory load-path))
	dependencies)
    (while addons
      (require (car addons))		;error will be reported here
      (let* ((requires-function
	      (intern-soft
	       (concat (symbol-name (car addons)) "-requires")))
	     (requires
	      (if (and requires-function
		       (fboundp requires-function))
		  (funcall requires-function)))
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
	      (setcdr (cdr entry) (cons (car addons) (nthcdr 2 entry)))
	    (setq dependencies (cons (list (car pointer) 0 (car addons))
				     dependencies)))
	  (setq pointer (cdr pointer))))
      (setq addons (cdr addons)))
    dependencies))

(defun riece-sort-addon-dependencies (dependencies)
  (let ((pointer dependencies)
	addons queue)
    (while pointer
      (if (zerop (nth 1 (car pointer)))
	  (setq dependencies (delq (car pointer) dependencies)
		queue (cons (car pointer) queue)))
      (setq pointer (cdr pointer)))
    (while queue
      (setq addons (cons (cons (car (car queue)) (nthcdr 2 (car queue)))
			 addons)
	    pointer (nthcdr 2 (car queue)))
      (while pointer
	(let* ((entry (assq (car pointer) dependencies))
	       (count (1- (nth 1 entry))))
	  (if (zerop count)
	      (setq dependencies (delq entry dependencies)
		    queue (nconc queue (list entry)))
	    (setcar (cdr entry) count)))
	(setq pointer (cdr pointer)))
      (setq queue (cdr queue)))
    (if dependencies
	(error "Circular add-on dependency found: %S" dependencies))
    (nreverse addons)))

(defun riece-resolve-addons (addons)
  ;; Add files in riece-addon-directory to addons.
  (if (file-directory-p riece-addon-directory)
      (setq addons (nconc
		    addons
		    (mapcar
		     (lambda (name)
		       (unless (file-directory-p
				(expand-file-name name riece-addon-directory))
			 (intern (file-name-sans-extension name))))
		     (directory-files riece-addon-directory nil "\\`[^.]")))))
  ;; Sort & uniquify.
  (setq addons (sort addons (lambda (symbol1 symbol2)
			      (string-lessp (symbol-name symbol1)
					    (symbol-name symbol2)))))
  (let ((pointer addons))
    (while pointer
      (if (memq (car pointer) (cdr pointer))
	  (setcar pointer nil))
      (setq pointer (cdr pointer)))
    (delq nil addons))
  ;; Build & resolve dependencies.
  (riece-sort-addon-dependencies
   (riece-load-and-build-addon-dependencies addons)))

(defun riece-insinuate-addon-1 (addon verbose)
  (if (get addon 'riece-addon-insinuated)
      (if verbose
	  (message (riece-mcat "Add-on %S is already insinuated") addon))
    (require addon)
    (funcall (intern (concat (symbol-name addon) "-insinuate")))
    (put addon 'riece-addon-insinuated t)
    (if verbose
	(message (riece-mcat "Add-on %S is insinuated") addon))
    (unless (get addon 'riece-addon-default-disabled)
      (riece-enable-addon addon t))))

(defun riece-insinuate-addon (addon &optional verbose)
  (unless (assq addon riece-addon-dependencies)
    (setq riece-addons (cons addon riece-addons)
	  riece-save-variables-are-dirty t
	  riece-addon-dependencies
	  (riece-resolve-addons
	   (cons addon (mapcar #'car riece-addon-dependencies)))))
  (let ((pointer riece-addon-dependencies)
	addons)
    (while pointer
      (unless (get (car (car pointer)) 'riece-addon-insinuated)
	(setq addons (cons (car (car pointer)) addons)))
      (if (eq (car (car pointer)) addon)
	  (setq pointer nil)
	(setq pointer (cdr pointer))))
    (setq addons (nreverse addons))
    (if (and (> (length addons) 1)
	     (eq verbose 'ask)
	     (not (y-or-n-p (format (riece-mcat
				     "%s will be insinuated.  Continue? ")
				    (mapconcat #'symbol-name addons ", ")))))
	(error "Insinuate operation was cancelled"))
    (while addons
      (riece-insinuate-addon-1 (car addons) verbose)
      (setq addons (cdr addons)))))

(defun riece-uninstall-addon (addon &optional verbose)
  (if (not (get addon 'riece-addon-insinuated))
      (if verbose
	  (message (riece-mcat "Add-on %S is not insinuated") addon))
    (let ((entry (assq addon riece-addon-dependencies))
	  (uninstall (intern-soft (concat (symbol-name addon) "-uninstall"))))
      (if entry
	  (if (cdr entry)
	      (if (= (length (cdr entry)) 1)
		  (error "%S depends on %S" (car (cdr entry)) addon)
		(error "%s depend on %S"
		       (mapconcat #'symbol-name (cdr entry) ", ")
		       addon))
	    (riece-disable-addon addon verbose)
	    (if (and uninstall
		     (fboundp uninstall))
		(funcall uninstall))
	    (setq riece-addon-dependencies
		  (delq entry riece-addon-dependencies))
	    (put addon 'riece-addon-insinuated nil)
	    (setq riece-addons (delq addon riece-addons)
		  riece-save-variables-are-dirty t
		  riece-addon-dependencies
		  (riece-resolve-addons
		   (delq addon (mapcar #'car riece-addon-dependencies))))))
      (if verbose
	  (message (riece-mcat "Add-on %S is uninstalled") addon)))))

(defun riece-enable-addon (addon &optional verbose)
  (unless (get addon 'riece-addon-insinuated)
    (error "Add-on %S is not insinuated" addon))
  (if (get addon 'riece-addon-enabled)
      (if verbose
	  (message (riece-mcat "Add-on %S is already enabled") addon))
    (let ((enable (intern-soft (concat (symbol-name addon) "-enable"))))
      (if (and enable
	       (fboundp enable))
	  (funcall enable))
      (put addon 'riece-addon-enabled t)
      (if verbose
	  (message (riece-mcat "Add-on %S enabled") addon)))))

(defun riece-disable-addon (addon &optional verbose)
  (unless (get addon 'riece-addon-insinuated)
    (error "Add-on %S is not insinuated" addon))
  (if (not (get addon 'riece-addon-enabled))
      (if verbose
	  (message (riece-mcat "Add-on %S is already disabled") addon))
    (let ((disable (intern-soft (concat (symbol-name addon) "-disable"))))
      (if (and disable
	       (fboundp disable))
	  (funcall disable))
      (put addon 'riece-addon-enabled nil)
      (if verbose
	  (message (riece-mcat "Add-on %S disabled") addon)))))

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
  (set-buffer (riece-get-buffer-create "*AddOn*" 'riece-addon-list-mode))
  (riece-addon-list-mode)
  (let ((inhibit-read-only t)
	buffer-read-only
	(pointer riece-addon-dependencies)
	module-description-alist
	description point longest)
    (while pointer
      (setq description (intern-soft (concat (symbol-name (car (car pointer)))
					     "-description"))
	    module-description-alist
	    (cons (cons (car (car pointer))
			(if (and description
				 (boundp description))
			    (riece-mcat (symbol-value description))
			  (riece-mcat "(no description)")))
		  module-description-alist)
	    pointer (cdr pointer)))
    (setq pointer riece-addon-modules)
    (while pointer
      (unless (assq (car (car pointer))
		    module-description-alist)
	(setq module-description-alist
	      (cons (cons (car (car pointer)) (riece-mcat (cdr (car pointer))))
		    module-description-alist)))
      (setq pointer (cdr pointer)))
    (erase-buffer)
    (riece-kill-all-overlays)
    (setq pointer module-description-alist
	  longest "")
    (while pointer
      (if (> (length (symbol-name (car (car pointer))))
	     (length longest))
	  (setq longest (symbol-name (car (car pointer)))))
      (setq pointer (cdr pointer)))
    (setq pointer (sort module-description-alist
			(lambda (entry1 entry2)
			  (string-lessp (symbol-name (car entry1))
					(symbol-name (car entry2))))))
    (while pointer
      (setq point (point))
      (insert (format (format "%%c %%-%dS %%s\n" (length longest))
		      (if (not (featurep (car (car pointer))))
			  ? 
			(if (not (get (car (car pointer))
				      'riece-addon-insinuated))
			    ??
			  (if (get (car (car pointer)) 'riece-addon-enabled)
			      ?+
			    ?-)))
		      (car (car pointer))
		      (cdr (car pointer))))
      (put-text-property point (point) 'riece-addon (car (car pointer)))
      (setq pointer (cdr pointer)))
    (insert (riece-mcat "
Symbols in the leftmost column:

   +     The add-on is enabled.
   -     The add-on is disabled.
   ?     The add-on is not insinuated.
         The add-on is not loaded.
"))
    (insert (substitute-command-keys (riece-mcat "
Useful keys:

   `\\[riece-command-enable-addon]' to enable the current add-on.
   `\\[riece-command-disable-addon]' to disable the current add-on.
   `\\[riece-command-insinuate-addon]' to insinuate the current add-on.
   `\\[riece-command-uninstall-addon]' to uninstall the current add-on.
   `\\[riece-command-unload-addon]' to unload the current add-on.
   `\\[riece-command-save-variables]' to save the current setting.
")))
    (goto-char (point-min))
    (pop-to-buffer (current-buffer))
    (delete-other-windows)))

(defun riece-addon-list-set-point (addon)
  (let ((point (point-min)))
    (while (and (not (eq (get-text-property point 'riece-addon) addon))
		(setq point (next-single-property-change point
							 'riece-addon))))
    (if point
	(goto-char point))))

(defun riece-command-enable-addon (addon)
  (interactive
   (list
    (or (if (eq major-mode 'riece-addon-list-mode)
	    (get-text-property (point) 'riece-addon))
	(intern-soft
	 (completing-read (riece-mcat "Add-on: ")
			  (mapcar (lambda (dependency)
				    (list (symbol-name (car dependency))))
				  riece-addon-dependencies)
			  (lambda (pointer)
			    (let ((enabled
				   (intern-soft (concat (car pointer)
							"-enabled"))))
			      (and enabled
				   (null (symbol-value enabled)))))
			  t)))))
  (riece-command-insinuate-addon addon)
  (riece-enable-addon addon t)
  (when (eq major-mode 'riece-addon-list-mode)
    (riece-command-list-addons)
    (riece-addon-list-set-point addon)))

(defun riece-command-disable-addon (addon)
  (interactive
   (list
    (or (if (eq major-mode 'riece-addon-list-mode)
	    (get-text-property (point) 'riece-addon))
	(intern-soft
	 (completing-read (riece-mcat "Add-on: ")
			  (mapcar (lambda (dependency)
				    (list (symbol-name (car dependency))))
				  riece-addon-dependencies)
			  (lambda (pointer)
			    (let ((enabled
				   (intern-soft (concat (car pointer)
							"-enabled"))))
			      (and enabled
				   (symbol-value enabled))))
			  t)))))
  (riece-disable-addon addon t)
  (when (eq major-mode 'riece-addon-list-mode)
    (riece-command-list-addons)
    (riece-addon-list-set-point addon)))

(defun riece-command-insinuate-addon (addon)
  (interactive
   (list
    (or (if (eq major-mode 'riece-addon-list-mode)
	    (get-text-property (point) 'riece-addon))
	(intern-soft
	 (completing-read (riece-mcat "Add-on: ")
			  (mapcar (lambda (dependency)
				    (list (symbol-name (car dependency))))
				  riece-addon-modules)
			  (lambda (pointer)
			    (not (get (intern-soft (car pointer))
				      'riece-addon-insinuated)))
			  t)))))
  (riece-insinuate-addon addon 'ask)
  (when (eq major-mode 'riece-addon-list-mode)
    (riece-command-list-addons)
    (riece-addon-list-set-point addon)))

(defun riece-command-uninstall-addon (addon)
  (interactive
   (list
    (or (if (eq major-mode 'riece-addon-list-mode)
	    (get-text-property (point) 'riece-addon))
	(intern-soft
	 (completing-read (riece-mcat "Add-on: ")
			  (mapcar (lambda (dependency)
				    (list (symbol-name (car dependency))))
				  riece-addon-dependencies)
			  (lambda (pointer)
			    (get (intern-soft (car pointer))
				 'riece-addon-insinuated))
			  t)))))
  (riece-uninstall-addon addon t)
  (when (eq major-mode 'riece-addon-list-mode)
    (riece-command-list-addons)
    (riece-addon-list-set-point addon)))

(defun riece-command-unload-addon (addon)
  (interactive
   (list
    (or (if (eq major-mode 'riece-addon-list-mode)
	    (get-text-property (point) 'riece-addon))
	(intern-soft
	 (completing-read (riece-mcat "Add-on: ")
			  (mapcar (lambda (dependency)
				    (list (symbol-name (car dependency))))
				  riece-addon-dependencies)
			  (lambda (pointer)
			    (get (intern-soft (car pointer))
				 'riece-addon-insinuated))
			  t)))))
  (riece-uninstall-addon addon t)
  (if (get addon 'riece-addon-not-unloadable)
      (message (riece-mcat "Add-on %S is not allowed to unload") addon)
    (unload-feature addon)
    (message (riece-mcat "Add-on %S is unloaded") addon))
  (when (eq major-mode 'riece-addon-list-mode)
    (riece-command-list-addons)
    (riece-addon-list-set-point addon)))

(provide 'riece-addon)

;;; riece-addon.el ends here
