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
	    (funcall (intern (concat (symbol-name (car addons)) "-disable")))
	    (if riece-debug
		(message "Add-on %S disabled" (car addons))))
	(if riece-debug
	    (message "Can't disable add-on %S" addon))))))

(provide 'riece-addon)

;;; riece-addon.el ends here
