;;; riece-xemacs.el --- XEmacs specific functions -*- lexical-binding: t -*-
;; Copyright (C) 1998-2003 Daiki Ueno

;; Author: Daiki Ueno <ueno@unixuser.org>
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

(defun riece-xemacs-hide-modeline ()
  "Remove modeline from current window."
  (set-specifier has-modeline-p nil (current-buffer)))

(when (featurep 'scrollbar)
  (defun riece-xemacs-hide-scrollbars ()
    (if (boundp 'horizontal-scrollbar-visible-p)
	(set-specifier horizontal-scrollbar-visible-p nil (current-buffer))
      (if (boundp 'scrollbar-height)
	  (set-specifier scrollbar-height 0 (current-buffer)))))
  (add-hook 'riece-user-list-mode-hook 'riece-xemacs-hide-scrollbars)
  (add-hook 'riece-channel-list-mode-hook 'riece-xemacs-hide-scrollbars))

(add-hook 'riece-user-list-mode-hook 'riece-xemacs-hide-modeline)
(add-hook 'riece-channel-list-mode-hook 'riece-xemacs-hide-modeline)

(defun riece-xemacs-simplify-modeline-format ()
  "Remove unnecessary information from `modeline-format'."
  (setq modeline-format
	(remrassq 'modeline-modified
		  (delq 'modeline-multibyte-status
			(copy-sequence mode-line-format)))))

(defalias 'riece-simplify-mode-line-format
  'riece-xemacs-simplify-modeline-format)

;;; stolen (and renamed) from gnus-xmas.el.
;;; In GNU Emacs, user can intercept whole mouse tracking events by
;;; assigning [mouse-X].  In XEmacs, however, which causes different
;;; effect, that is, the command assigned to [mouse-X] only catches
;;; button-release events.
(defvar riece-mouse-2 [button2])

;;; popup-menu compatibility stuff, stolen (and renamed) from
;;; semi-def.el.
(defun riece-popup-menu-popup (menu event)
  (let ((response (get-popup-menu-response menu event)))
    (if response
	(funcall (event-function response) (event-object response)))))

(defalias 'riece-event-buffer 'event-buffer)
(defalias 'riece-event-point 'event-point)

;;; stolen (and renamed) from gnus-xmas.el.
(defalias 'riece-region-active-p 'region-active-p)

(defalias 'riece-make-overlay 'make-extent)
(defalias 'riece-overlay-put 'set-extent-property)
(defalias 'riece-overlay-start 'extent-start-position)
(defalias 'riece-overlay-buffer 'extent-buffer)

(defun riece-overlays-in (start end)
  (extent-list (current-buffer) start end))

(defalias 'riece-delete-overlay 'delete-extent)

(defun riece-kill-all-overlays ()
  "Delete all extents in the current buffer."
  (map-extents (lambda (extent ignore)
		 (delete-extent extent)
		 nil)))

;;; stolen (and renamed) from nnheaderxm.el.
(defun riece-xemacs-generate-timer-name (&optional prefix)
  (let ((counter '(0)))
    (format "%s-%d"
	    (or prefix
		"riece-xemacs-timer")
	    (prog1 (car counter)
	      (setcar counter (1+ (car counter)))))))

(defun riece-run-at-time (time repeat function &rest args)
  (let ((name (riece-xemacs-generate-timer-name "riece-run-at-time")))
    (start-itimer
     name
     `(lambda ()
	(,function ,@args))
     time repeat)
    name))

(defun riece-run-with-idle-timer (time repeat function &rest args)
  (let ((name (riece-xemacs-generate-timer-name "riece-run-with-idle-timer")))
    (start-itimer
     name
     `(lambda ()
	(,function ,@args))
     time (if repeat 1) t)
    name))

(defalias 'riece-cancel-timer 'delete-itimer)

(defun riece-match-string-no-properties (number &optional string)
  (format "%s" (match-string number string)))

(defun riece-propertize-modeline-string (string &rest properties)
  (let ((extent (make-extent nil nil)))
    (set-extent-properties extent properties)
    (cons extent string)))

(defalias 'riece-normalize-modeline-string 'identity)

(defalias 'riece-facep 'find-face)

(defun riece-put-text-property-nonsticky (start end prop value
						     &optional object)
  (add-text-properties start end (list prop value 'start-open t 'end-open t)
		       object))

(defun riece-recent-messages (n)
  "Return N most recent messages, most recent first.
If N is nil, all messages will be returned."
  (with-output-to-string
    (print-recent-messages n)))

(defalias 'riece-remprop 'remprop)

(provide 'riece-xemacs)

;;; riece-xemacs.el ends here
