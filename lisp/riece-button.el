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

(defconst riece-channel-button-popup-menu
  '("Channel"
    ["Switch To Channel" riece-channel-button-switch-to-channel]
    ["Part Channel" riece-channel-button-part]
    ["List Channel" riece-channel-button-list])
  "Menu for channel buttons.")

(defconst riece-user-button-popup-menu
  '("User"
    ["Start Private Conversation" riece-user-button-join-partner]
    ["Give Channel Operator Privileges" riece-user-button-set-operators]
    ["Allow To Speak" riece-user-button-set-speakers]
    ["Finger (WHOIS)" riece-user-button-finger])
  "Menu for user buttons.")

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
    (format "%S: switch to %s; down-mouse-3: more options"
	    (aref riece-mouse-2 0)
	    ;; XEmacs will get a single widget arg; Emacs 21 will get
	    ;; window, overlay, position.
	    (riece-format-identity
	     (if overlay
		 (with-current-buffer (overlay-buffer overlay)
		   (widget-value (widget-at (overlay-start overlay))))
	       (widget-value widget/window))))))

(defun riece-button-switch-to-identity (widget &optional event)
  "Switch to identity stored in WIDGET.
This function is used as a callback for a channel button."
  (let ((channel (widget-value widget)))
    (if (riece-identity-member channel riece-current-channels)
	(riece-command-switch-to-channel channel)
      (message "%s" (substitute-command-keys
		     "Type \\[riece-command-join] to join the channel")))))

(defun riece-identity-button-popup-menu (event)
  "Popup the menu for identity buttons."
  (interactive "@e")
  (save-excursion
    (set-buffer (riece-event-buffer event))
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
  (let (group)
    (if (riece-region-active-p)
	(save-excursion
	  (riece-button-map-identity-region
	   (region-beginning) (region-end)
	   (lambda (start end)
	     (setq group (cons (get-text-property start 'riece-identity)
			       group)))))
      (setq group (list (get-text-property (point) 'riece-identity))))
    (if (setq group
	      (delq nil
		    (mapcar
		     (lambda (identity)
		       (riece-with-server-buffer (riece-identity-server
						  riece-current-channel)
			 (if (and (member
				   (riece-identity-prefix identity)
				   (riece-channel-get-users
				    (riece-identity-prefix
				     riece-current-channel)))
				  (not (member
					(riece-identity-prefix identity)
					(riece-channel-get-operators
					 (riece-identity-prefix
					  riece-current-channel)))))
			     identity)))
		     group)))
	(riece-command-set-operators (mapcar #'riece-identity-prefix group)))))

(defun riece-user-button-set-speakers ()
  (interactive)
  (let (group)
    (if (riece-region-active-p)
	(save-excursion
	  (riece-button-map-identity-region
	   (region-beginning) (region-end)
	   (lambda (start end)
	     (setq group (cons (get-text-property start 'riece-identity)
			       group)))))
      (setq group (list (get-text-property (point) 'riece-identity))))
    (if (setq group
	      (delq nil
		    (mapcar
		     (lambda (identity)
		       (riece-with-server-buffer (riece-identity-server
						  riece-current-channel)
			 (if (and (member
				   (riece-identity-prefix identity)
				   (riece-channel-get-users
				    (riece-identity-prefix
				     riece-current-channel)))
				  (not (member
					(riece-identity-prefix identity)
					(riece-channel-get-operators
					 (riece-identity-prefix
					  riece-current-channel))))
				  (not (member
					(riece-identity-prefix identity)
					(riece-channel-get-speakers
					 (riece-identity-prefix
					  riece-current-channel)))))
			     identity)))
		     group)))
	(riece-command-set-speakers (mapcar #'riece-identity-prefix group)))))

(defun riece-user-button-finger ()
  (interactive)
  (riece-command-finger
   (riece-identity-prefix (get-text-property (point) 'riece-identity))))

(defun riece-make-identity-button-map ()
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map (current-local-map))
    (define-key map [down-mouse-3] 'riece-identity-button-popup-menu)
    map))

(defun riece-button-map-identity-region (start end function)
  (catch 'done
    (while t
      ;; Search for the beginning of the button region.
      (unless (get-text-property start 'riece-identity)
	(setq start (next-single-property-change start 'riece-identity
						 nil end)))
      (if (= start end)
	  (throw 'done nil))
      ;; Search for the end of the button region.
      (let ((button-end (next-single-property-change start 'riece-identity
						     nil end)))
	(if (= button-end end)
	    (throw 'done nil))
	(funcall function start button-end)
	(setq start button-end)))))

(defvar riece-identity-button-map)
(defun riece-button-add-identity-button (start end)
  (riece-button-map-identity-region
   start end
   (lambda (start end)
     (let ((inhibit-read-only t)
	   buffer-read-only)
       (widget-convert-button 'riece-identity-button start end
			      (get-text-property start 'riece-identity))
       (add-text-properties start end
			    (list 'local-map riece-identity-button-map
				  'keymap riece-identity-button-map))))))

(defun riece-button-update-channel-list-buffer ()
  (save-excursion
    (set-buffer riece-channel-list-buffer)
    (riece-button-add-identity-button (point-min) (point-max))))

(defun riece-button-update-user-list-buffer ()
  (save-excursion
    (set-buffer riece-user-list-buffer)
    (riece-button-add-identity-button (point-min) (point-max))))

(defun riece-button-requires ()
  '(riece-highlight))

(defvar riece-channel-list-mode-map)
(defvar riece-user-list-mode-map)
(defvar riece-dialogue-mode-map)
(defun riece-button-insinuate ()
  (add-hook 'riece-update-buffer-functions
	    'riece-button-update-channel-list-buffer t)
  (add-hook 'riece-update-buffer-functions
	    'riece-button-update-user-list-buffer t)
  (add-hook 'riece-channel-list-mode-hook
	    (lambda ()
	      (set-keymap-parent riece-channel-list-mode-map widget-keymap)
	      (set (make-local-variable 'riece-identity-button-map)
		   (riece-make-identity-button-map))))
  (add-hook 'riece-user-list-mode-hook
	    (lambda ()
	      (set-keymap-parent riece-user-list-mode-map widget-keymap)
	      (set (make-local-variable 'riece-identity-button-map)
		   (riece-make-identity-button-map))))
  (add-hook 'riece-dialogue-mode-hook
	    (lambda ()
	      (set-keymap-parent riece-dialogue-mode-map widget-keymap)
	      (set (make-local-variable 'riece-identity-button-map)
		   (riece-make-identity-button-map))))
  (add-hook 'riece-after-insert-functions 'riece-button-add-identity-button))

(provide 'riece-button)

;;; riece-button.el ends here
