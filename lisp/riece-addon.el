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

(defvar riece-addon-list-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "+" 'riece-command-enable-addon)
    (define-key keymap "-" 'riece-command-disable-addon)
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

(defun riece-resolve-addons (addons)
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

(defun riece-insinuate-addon (addon)
  (require addon)		;implicit dependency
  (funcall (intern (concat (symbol-name addon) "-insinuate")))
  (if riece-debug
      (message "Add-on %S is insinuated" addon)))

(defun riece-enable-addon (addon)
  (let ((enabled (intern-soft (concat (symbol-name addon) "-enabled"))))
    (if (null enabled)
	(if riece-debug
	    (message "Add-on %S doesn't support enable/disable" addon))
      (if (symbol-value enabled)
	  (if riece-debug
	      (message "Can't enable add-on %S" addon))
	(funcall (intern (concat (symbol-name addon) "-enable")))
	(if riece-debug
	    (message "Add-on %S enabled" addon))))))

(defun riece-disable-addon (addon)
  (let ((enabled (intern-soft (concat (symbol-name addon) "-enabled"))))
    (if (null enabled)
	(if riece-debug
	    (message "Add-on %S doesn't support enable/disable" addon))
      (if (symbol-value enabled)
	  (progn
	    (funcall (intern (concat (symbol-name addon) "-disable")))
	    (if riece-debug
		(message "Add-on %S disabled" addon)))
	(if riece-debug
	    (message "Can't disable add-on %S" addon))))))

(defun riece-addon-list-mode ()
  "Major mode for displaying addon list.
All normal editing commands are turned off."
  (kill-all-local-variables)
  (buffer-disable-undo)
  (setq major-mode 'riece-addon-list-mode
        mode-name "AddOns"
	mode-line-buffer-identification
	(riece-mode-line-buffer-identification '("Riece: "))
	truncate-lines t
	buffer-read-only t)
  (use-local-map riece-addon-list-mode-map)
  (run-hooks 'riece-addon-list-mode-hook))

(defun riece-command-list-addons ()
  (interactive)
  (save-excursion
    (set-buffer (riece-get-buffer-create "*AddOns*" 'riece-addon-list-mode))
    (riece-addon-list-mode)
    (let ((inhibit-read-only t)
	  buffer-read-only
	  (pointer riece-addons)
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
			      ?=
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
  =	The add-on doesn't support enable/disable operation.
  ?	The add-on status is not known.
"))
    (pop-to-buffer (current-buffer))))

(defun riece-command-enable-addon (addon)
  (interactive
   (list
    (or (if (eq major-mode 'riece-addon-list-mode)
	    (get-text-property (point) 'riece-addon))
	(completing-read "Add-on: "
			 (mapcar #'list riece-addons)
			 (lambda (pointer)
			   (setq enabled (intern-soft (concat (car pointer)
							      "-enabled")))
			   (and enabled
				(null (symbol-value enabled))))
			 t))))
  (riece-enable-addon addon)
  (riece-command-list-addons))

(defun riece-command-disable-addon (addon)
  (interactive
   (list
    (or (if (eq major-mode 'riece-addon-list-mode)
	    (get-text-property (point) 'riece-addon))
	(completing-read "Add-on: "
			 (mapcar #'list riece-addons)
			 (lambda (pointer)
			   (setq enabled (intern-soft (concat (car pointer)
							      "-enabled")))
			   (and enabled
				(symbol-value enabled)))
			 t))))
  (riece-disable-addon addon)
  (riece-command-list-addons))
      
(provide 'riece-addon)

;;; riece-addon.el ends here
