;;; riece-button.el --- adding buttons in channel buffers
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

;; To use, add the following line to your ~/.riece/init.el:
;; (add-to-list 'riece-addons 'riece-button)

;;; Code:

(require 'riece-commands)
(require 'riece-identity)
(require 'riece-misc)
(require 'wid-edit)

(defvar help-echo-owns-message)
(define-widget 'riece-channel-button 'push-button
  "A channel button."
  :action 'riece-channel-button-action
  :help-echo
  (lambda (widget/window &optional overlay pos)
    ;; Needed to properly clear the message due to a bug in
    ;; wid-edit (XEmacs only).
    (if (boundp 'help-echo-owns-message)
	(setq help-echo-owns-message t))
    (format "Switch to %s"
	    ;; XEmacs will get a single widget arg; Emacs 21 will get
	    ;; window, overlay, position.
	    (riece-format-identity
	     (if overlay
		 (with-current-buffer (overlay-buffer overlay)
		   (widget-value (widget-at (overlay-start overlay))))
	       (widget-value widget/window))))))

(defun riece-channel-button-action (widget &optional event)
  (let ((channel (widget-value widget)))
    (if (riece-identity-member channel riece-current-channels)
	(riece-command-switch-to-channel channel)
      (message "%s" (substitute-command-keys
		     "Type \\[riece-command-join] to join the channel")))))

(defun riece-button-add-channel-buttons (start end length)
  (save-excursion
    (catch 'done
      (while t
	;; Search for the beginning of the button region.
	(unless (get-text-property start 'riece-identity)
	  (setq start (next-single-property-change start 'riece-identity
						   nil end)))
	;; Search for the end of the button region.
	(let* ((identity (get-text-property start 'riece-identity))
	       (button-end (next-single-property-change start 'riece-identity
							nil end)))
	  (if (= button-end end)
	      (throw 'done nil)
	    (if (riece-channel-p (riece-identity-prefix identity))
		(widget-convert-button
		 'riece-channel-button start button-end identity))
	    (setq start button-end)))))))

(defun riece-button-update-channel-list-buffer ()
  (if riece-channel-list-buffer-mode
      (save-excursion
	(set-buffer riece-channel-list-buffer)
	(let ((inhibit-read-only t)
	      buffer-read-only)
	  (riece-button-add-channel-buttons (point-min) (point-max) nil)))))

(defun riece-button-requires ()
  '(riece-highlight))

(defun riece-button-insinuate ()
  (add-hook 'riece-channel-list-mode-hook
	    (lambda ()
	      (set-keymap-parent riece-channel-list-mode-map widget-keymap)
	      (add-hook 'riece-update-buffer-functions
			'riece-button-update-channel-list-buffer t))))

(provide 'riece-button)

;;; riece-button.el ends here
