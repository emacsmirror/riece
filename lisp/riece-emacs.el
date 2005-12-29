;;; riece-emacs.el --- FSF Emacs specific functions
;; Copyright (C) 1999 Daiki Ueno

;; Author: Daiki Ueno <ueno@unixuser.org>
;; Created: 1999-08-21
;; Keywords: emulation

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

;;; stolen (and renamed) from gnus-ems.el.
;;; In GNU Emacs, user can intercept whole mouse tracking events by
;;; assigning [mouse-X].  In XEmacs, however, which causes different
;;; effect, that is, the command assigned to [mouse-X] only catches
;;; button-release events.
(defvar riece-mouse-2 [mouse-2])

;;; popup-menu compatibility stuff, stolen (and renamed) from
;;; semi-def.el.
(defmacro riece-popup-menu-bogus-filter-constructor (menu)
  ;; #### Kludge for FSF Emacs-style menu.
  (let ((bogus-menu (make-symbol "bogus-menu")))
    `(let (,bogus-menu selection function)
       (easy-menu-define ,bogus-menu nil nil ,menu)
       (setq selection (x-popup-menu t ,bogus-menu))
       (when selection
	 (setq function (lookup-key ,bogus-menu (apply #'vector selection)))
	 ;; If a callback entry has no name, easy-menu wraps its value.
	 ;; See `easy-menu-make-symbol'.
	 (if (eq t (compare-strings "menu-function-" 0 nil
				    (symbol-name function) 0 14))
	     (car (last (symbol-function function)))
	   function)))))

(defun riece-popup-menu-popup (menu event)
  (let ((function (riece-popup-menu-bogus-filter-constructor menu)))
    (if function
	(funcall function))))

(defun riece-event-buffer (event)
  "Return the buffer of the window over which mouse event EVENT occurred."
  (window-buffer (posn-window (event-start event))))

(defun riece-event-point (event)
  "Return the character position of the mouse event EVENT."
  (posn-point (event-start event)))

;;; stolen (and renamed) from gnus-ems.el.
(defun riece-region-active-p ()
  "Say whether the region is active."
  (and (boundp 'transient-mark-mode)
       transient-mark-mode
       (boundp 'mark-active)
       mark-active))

(defalias 'riece-make-overlay 'make-overlay)
(defalias 'riece-overlay-put 'overlay-put)
(defalias 'riece-overlay-start 'overlay-start)
(defalias 'riece-overlay-buffer 'overlay-buffer)
(defalias 'riece-overlays-in 'overlays-in)
(defalias 'riece-delete-overlay 'delete-overlay)

(defun riece-kill-all-overlays ()
  "Delete all overlays in the current buffer."
  (let* ((overlay-lists (overlay-lists))
	 (buffer-read-only nil)
	 (overlays (delq nil (nconc (car overlay-lists) (cdr overlay-lists)))))
    (while overlays
      (delete-overlay (car overlays))
      (setq overlays (cdr overlays)))))

(defalias 'riece-run-at-time 'run-at-time)
(defalias 'riece-run-with-idle-timer 'run-with-idle-timer)
(defalias 'riece-cancel-timer 'cancel-timer)

(defalias 'riece-match-string-no-properties 'match-string-no-properties)

(defun riece-propertize-modeline-string (string &rest properties)
  (add-text-properties 0 (length string) properties string)
  string)

(defun riece-normalize-modeline-string-1 (string)
  (if string
      (if (listp (car string))
	  (cons (car (car string)) (riece-normalize-modeline-string-1
				    (append (cdr (car string)) (cdr string))))
	(cons (car string) (riece-normalize-modeline-string-1
			    (cdr string))))))

(defun riece-normalize-modeline-string (string)
  (if (listp string)
      (list (apply #'concat (riece-normalize-modeline-string-1 string)))
    string))

(defun riece-put-text-property-nonsticky (start end prop value
						     &optional object)
  (add-text-properties start end
		       (list prop value 'front-sticky nil 'rear-nonsticky t)
		       object))

(defalias 'riece-facep 'facep)

;;; stolen (and renamed) from emacsbug.el.
(defun riece-recent-messages (n)
  "Return N most recent messages, most recent first.
If N is nil, all messages will be returned."
  (let ((message-buf (get-buffer "*Messages*")))
    (if message-buf
	(with-temp-buffer
	  (let (beg-pos end-pos)
	    (with-current-buffer message-buf
	      (setq end-pos (goto-char (point-max)))
	      (if n
		  (progn
		    (forward-line (- n))
		    (setq beg-pos (point)))
		(setq beg-pos (point-min))))
	    (insert-buffer-substring message-buf beg-pos end-pos)
	    (reverse-region (point-min) (point-max))
	    (buffer-string))))))

(defun riece-remprop (symbol property)
  (let ((plist (symbol-plist symbol)))
    (if (eq (car plist) property)
	(setplist symbol (cdr (cdr plist)))
      (while (and (nthcdr 2 plist)
		  (not (eq (car (nthcdr 2 plist)) property)))
	(setq plist (nthcdr 2 plist)))
      (if (nthcdr 2 plist)
	  (setcdr (cdr plist) (nthcdr 4 plist))))))

(provide 'riece-emacs)

;;; riece-emacs.el ends here
