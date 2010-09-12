;;; riece-button.el --- display useful buttons in IRC buffers
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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; NOTE: This is an add-on module for Riece.

;;; Code:

(require 'riece-commands)
(require 'riece-identity)
(require 'riece-misc)
(require 'wid-edit)

(defconst riece-channel-button-popup-menu
  '("Channel"
    ["Switch To Channel" riece-channel-button-switch-to-channel]
    ["Part Channel" riece-channel-button-part]
    ["List Channel" riece-channel-button-list])
  "Menu for channel buttons.")

(defconst riece-user-button-popup-menu
  '("User"
    ["Finger (WHOIS)" riece-user-button-finger]
    ["Start Private Conversation" riece-user-button-join-partner]
    ["Set +o" riece-user-button-set-operators]
    ["Set +v" riece-user-button-set-speakers])
  "Menu for user buttons.")

(defconst riece-button-description
  "Display useful buttons in IRC buffers.")

(defvar help-echo-owns-message)
(define-widget 'riece-identity-button 'push-button
  "A channel button."
  :action 'riece-button-switch-to-identity
  :help-echo
  (lambda (widget/window &optional overlay pos)
    ;; Needed to properly clear the message due to a bug in
    ;; wid-edit (XEmacs only).
    (if (boundp 'help-echo-owns-message)
	(setq help-echo-owns-message t))
    (format (riece-mcat "%S: switch to %s; down-mouse-3: more options")
	    (aref riece-mouse-2 0)
	    ;; XEmacs will get a single widget arg; Emacs 21 will get
	    ;; window, overlay, position.
	    (riece-format-identity
	     (if overlay
		 (with-current-buffer (riece-overlay-buffer overlay)
		   (widget-value (widget-at (riece-overlay-start overlay))))
	       (widget-value widget/window))))))

(defun riece-button-switch-to-identity (widget &optional event)
  "Switch to identity stored in WIDGET.
This function is used as a callback for a channel button."
  (let ((channel (widget-value widget)))
    (if (riece-identity-member channel riece-current-channels)
	(riece-command-switch-to-channel channel)
      (message "%s" (substitute-command-keys
		     (riece-mcat
		      "Type \\[riece-command-join] to join the channel"))))))

(defun riece-identity-button-click (event)
  "Call widget-button-click and select the last selected window."
  (interactive "e")			;widget-button-click has
					;interactive spec "@e"
  (let ((buffer (current-buffer))
	(point (point))
	window)
    (unwind-protect
	(with-current-buffer (riece-event-buffer event)
	  (goto-char (riece-event-point event))
	  (widget-button-click event))
      ;; riece-button-switch-to-identity changes window-configuration
      ;; so we must select the last selected window by _buffer_.
      (if (setq window (get-buffer-window buffer))
	  (progn
	    (select-window window)
	    (set-window-point window point))
	(if riece-debug
	    (riece-debug (format "buffer %s not visible"
				 (buffer-name buffer))))))))

(defun riece-identity-button-popup-menu (event)
  "Popup the menu for identity buttons."
  (interactive "e")
  (with-current-buffer (riece-event-buffer event)
    (goto-char (riece-event-point event))
    (riece-popup-menu-popup
     (if (riece-channel-p (riece-identity-prefix
			   (get-text-property (point) 'riece-identity)))
	 riece-channel-button-popup-menu
       riece-user-button-popup-menu)
     event)))

(defun riece-channel-button-switch-to-channel ()
  (interactive)
  (riece-command-switch-to-channel
   (get-text-property (point) 'riece-identity)))

(defun riece-channel-button-part ()
  (interactive)
  (riece-command-part
   (get-text-property (point) 'riece-identity)))

(defun riece-channel-button-list ()
  (interactive)
  (riece-command-list
   (riece-identity-prefix (get-text-property (point) 'riece-identity))))

(defun riece-user-button-join-partner ()
  (interactive)
  (riece-command-join-partner
   (get-text-property (point) 'riece-identity)))

(defun riece-user-button-set-operators ()
  (interactive)
  (let (group users)
    (if (riece-region-active-p)
	(save-excursion
	  (riece-scan-property-region
	   'riece-identity
	   (region-beginning) (region-end)
	   (lambda (start end)
	     (setq group (cons (get-text-property start 'riece-identity)
			       group)))))
      (setq group (list (get-text-property (point) 'riece-identity))))
    (setq users (riece-with-server-buffer
		    (riece-identity-server riece-current-channel)
		  (riece-channel-get-users (riece-identity-prefix
					    riece-current-channel))))
    (if (setq group
	      (delq nil
		    (mapcar
		     (lambda (identity)
		       (unless (memq ?o (cdr (riece-identity-assoc
					      (riece-identity-prefix identity)
					      users
					      t)))
			 identity))
		     group)))
	(riece-command-set-operators (mapcar #'riece-identity-prefix group)))))

(defun riece-user-button-set-speakers ()
  (interactive)
  (let (group users)
    (if (riece-region-active-p)
	(save-excursion
	  (riece-scan-property-region
	   'riece-identity
	   (region-beginning) (region-end)
	   (lambda (start end)
	     (setq group (cons (get-text-property start 'riece-identity)
			       group)))))
      (setq group (list (get-text-property (point) 'riece-identity))))
    (setq users (riece-with-server-buffer
		    (riece-identity-server riece-current-channel)
		  (riece-channel-get-users (riece-identity-prefix
					    riece-current-channel))))
    (if (setq group
	      (delq nil
		    (mapcar
		     (lambda (identity)
		       (unless (memq ?v (cdr (riece-identity-assoc
					      (riece-identity-prefix identity)
					      users
					      t)))
			 identity))
		     group)))
	(riece-command-set-speakers (mapcar #'riece-identity-prefix group)))))

(defun riece-user-button-finger ()
  (interactive)
  (riece-command-finger (get-text-property (point) 'riece-identity)))

(defun riece-make-identity-button-map ()
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map (current-local-map))
    (define-key map [down-mouse-2] 'riece-identity-button-click)
    (define-key map [down-mouse-3] 'riece-identity-button-popup-menu)
    map))

(defvar riece-identity-button-map)
(defun riece-button-add-identity-button (start end)
  (if (get 'riece-button 'riece-addon-enabled)
      (riece-scan-property-region
       'riece-identity
       start end
       (lambda (start end)
	 (let ((inhibit-read-only t)
	       buffer-read-only)
	   (widget-convert-button 'riece-identity-button start end
				  (get-text-property start 'riece-identity))
	   (add-text-properties
	    start end
	    (list 'local-map riece-identity-button-map
		  'keymap riece-identity-button-map)))))))

(defun riece-button-update-buffer ()
  (riece-button-add-identity-button (point-min) (point-max)))

(defvar riece-channel-list-mode-map)
(defvar riece-user-list-mode-map)
(defvar riece-dialogue-mode-map)

(defun riece-button-channel-list-mode-hook ()
  (set-keymap-parent riece-channel-list-mode-map widget-keymap)
  (set (make-local-variable 'riece-identity-button-map)
       (riece-make-identity-button-map))
  (add-hook 'riece-update-buffer-functions
	    'riece-button-update-buffer t t))

(defun riece-button-user-list-mode-hook ()
  (set-keymap-parent riece-user-list-mode-map widget-keymap)
  (set (make-local-variable 'riece-identity-button-map)
       (riece-make-identity-button-map))
  (add-hook 'riece-update-buffer-functions
	    'riece-button-update-buffer t t))

(defun riece-button-dialogue-mode-hook ()
  (set-keymap-parent riece-dialogue-mode-map widget-keymap)
  (set (make-local-variable 'riece-identity-button-map)
       (riece-make-identity-button-map)))

(defun riece-button-insinuate ()
  (save-excursion
    (when riece-channel-list-buffer
      (set-buffer riece-channel-list-buffer)
      (riece-button-channel-list-mode-hook))
    (when riece-user-list-buffer
      (set-buffer riece-user-list-buffer)
      (riece-button-user-list-mode-hook))
    (let ((buffers riece-buffer-list))
      (while buffers
	(set-buffer (car buffers))
	(if (riece-derived-mode-p 'riece-dialogue-mode)
	    (riece-button-dialogue-mode-hook))
	(setq buffers (cdr buffers)))))
  (add-hook 'riece-channel-list-mode-hook
	    'riece-button-channel-list-mode-hook)
  (add-hook 'riece-user-list-mode-hook
	    'riece-button-user-list-mode-hook)
  (add-hook 'riece-dialogue-mode-hook
	    'riece-button-dialogue-mode-hook)
  (add-hook 'riece-after-insert-functions 'riece-button-add-identity-button))

(defun riece-button-uninstall ()
  (let ((buffers riece-buffer-list))
    (save-excursion
      (while buffers
	(set-buffer (car buffers))
	(remove-hook 'riece-update-buffer-functions
		     'riece-button-update-buffer t)
	(if (local-variable-p 'riece-identity-button-map
			      (car buffers))
	    (kill-local-variable 'riece-identity-button-map))
	(setq buffers (cdr buffers)))))
  (remove-hook 'riece-channel-list-mode-hook
	       'riece-button-channel-list-mode-hook)
  (remove-hook 'riece-user-list-mode-hook
	       'riece-button-user-list-mode-hook)
  (remove-hook 'riece-dialogue-mode-hook
	       'riece-button-dialogue-mode-hook)
  (remove-hook 'riece-after-insert-functions
	       'riece-button-add-identity-button))

(defun riece-button-enable ()
  (let ((pointer riece-buffer-list))
    (while pointer
      (with-current-buffer (car pointer)
	(if (riece-derived-mode-p 'riece-dialogue-mode)
	    (riece-button-update-buffer)))
      (setq pointer (cdr pointer)))
    (if riece-current-channel
	(riece-emit-signal 'user-list-changed riece-current-channel))
    (riece-emit-signal 'channel-list-changed)))

(defun riece-button-disable ()
  (save-excursion
    (let ((pointer riece-buffer-list))
      (while pointer
	;; On XEmacs, BUFFER arg of widget-map-buttons is ignored.
	(set-buffer (car pointer))
	(widget-map-buttons
	 (lambda (widget maparg)
	   (widget-leave-text widget)))
	(setq pointer (cdr pointer))))))

(provide 'riece-button)

;;; riece-button.el ends here
