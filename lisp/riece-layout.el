;;; riece-layout.el --- layout management
;; Copyright (C) 1998-2003 Daiki Ueno

;; Author: Daiki Ueno <ueno@unixuser.org>
;;	TAKAHASHI "beatmaria" Kaoru <kaoru@kaisei.org>
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

(require 'riece-globals)
(require 'riece-misc)

(defgroup riece-layout nil
  "Manage window layouts"
  :prefix "riece-"
  :group 'riece)

(defcustom riece-layout "default"
  "Current layout setting."
  :type 'string
  :group 'riece-layout)

(defcustom riece-layout-alist
  '(("default" riece-configure-windows riece-reconfigure-windows-predicate)
    ("top" riece-configure-windows-top riece-reconfigure-windows-predicate))
  "An alist mapping the names to layout functions.
An element of this alist is in the following form:
(NAME CONFIGURE-FUNCTION RECONFIGURE-PREDICATE)
NAME is a string which specifies the layout setting.
CONFIGURE-FUNCTION is a function which does window splitting, etc.
RECONFIGURE-PREDICATE is a function to examine whether windows
reconfiguration is needed."
  :type 'list
  :group 'riece-layout)

(defun riece-redraw-layout (&optional force)
  "Reconfigure windows with the current layout.
If optional argument FORCE is non-nil, window reconfiguration will
happen unconditionally."
  (let ((layout (cdr (assoc riece-layout riece-layout-alist))))
    (unless layout
      (error "No such layout!"))
    (if (or force
	    (funcall (nth 1 layout)))
	(funcall (car layout)))))
  
(defun riece-set-window-points ()
  (if (get-buffer-window riece-user-list-buffer)
      (with-current-buffer riece-user-list-buffer
	(unless (riece-frozen riece-user-list-buffer)
	  (set-window-start (get-buffer-window riece-user-list-buffer)
			    (point-min)))))
  (if (get-buffer-window riece-channel-list-buffer)
      (with-current-buffer riece-channel-list-buffer
	(unless (riece-frozen riece-channel-list-buffer)
	  (set-window-start (get-buffer-window riece-channel-list-buffer)
			    (point-min))))))

(defun riece-configure-windows ()
  "Configure windows.
This function is used by \"default\" layout."
  (let ((buffer (window-buffer))
	(show-user-list
	 (and riece-user-list-buffer-mode
	      riece-current-channel
	      ;; User list buffer is nuisance for private conversation.
	      (riece-channel-p (riece-identity-prefix
				riece-current-channel)))))
    ;; Can't expand minibuffer to full frame.
    (if (eq (selected-window) (minibuffer-window))
	(other-window 1))
    (delete-other-windows)
    (if (and riece-current-channel
	     (or show-user-list riece-channel-list-buffer-mode))
	(let ((rest-window (split-window (selected-window)
					 (/ (window-width) 5) t)))
	  (if (and show-user-list riece-channel-list-buffer-mode)
	      (progn
		(set-window-buffer (split-window)
				   riece-channel-list-buffer)
		(set-window-buffer (selected-window)
				   riece-user-list-buffer))
	    (if show-user-list
		(set-window-buffer (selected-window)
				   riece-user-list-buffer)
	      (if riece-channel-list-buffer-mode
		  (set-window-buffer (selected-window)
				     riece-channel-list-buffer))))
	  (select-window rest-window)))
    (if (and riece-current-channel
	     riece-channel-buffer-mode)
	(let ((rest-window (split-window)))
	  (set-window-buffer (selected-window)
			     riece-channel-buffer)
	  (set-window-buffer (split-window rest-window 4)
			     riece-others-buffer)
	  (with-current-buffer riece-channel-buffer
	    (setq truncate-partial-width-windows nil))
	  (with-current-buffer riece-others-buffer
	    (setq truncate-partial-width-windows nil))
	  (set-window-buffer rest-window
			     riece-command-buffer))
      (set-window-buffer (split-window (selected-window) 4)
			 riece-dialogue-buffer)
      (set-window-buffer (selected-window)
			 riece-command-buffer))
    (riece-set-window-points)
    (select-window (or (get-buffer-window buffer)
		       (get-buffer-window riece-command-buffer)))))

(defun riece-reconfigure-windows-predicate ()
  "Return t, if window reconfiguration is needed.
This function is used by \"default\" layout."
  (memq (window-buffer (selected-window))
	riece-buffer-list))

(defun riece-configure-windows-top (&optional plist)
  "Candidate of `riece-configure-windows-function'.
PLIST accept :command-height, :user-list-width, and :channel-list-width."
  (let ((command-height (or (plist-get plist :command-height) 4))
	(user-list-width (or (plist-get plist :user-list-width) (+ 9 1 1)))
	(channel-list-width (or (plist-get plist :channel-list-width) 18))
	(buffer (window-buffer))
	(show-user-list
	 (and riece-user-list-buffer-mode
	      riece-current-channel
	      ;; User list buffer is nuisance for private conversation.
	      (riece-channel-p (riece-identity-prefix
				riece-current-channel)))))
    ;; Can't expand minibuffer to full frame.
    (when (eq (selected-window) (minibuffer-window))
      (other-window 1))
    (delete-other-windows)
    ;; top of frame
    (let ((rest-window (split-window (selected-window) command-height)))
      (set-window-buffer (selected-window)
			 riece-command-buffer)
      (select-window rest-window))
    ;; middle of frame (vertical-spilit when need)
    (when (or (and riece-current-channel riece-channel-buffer-mode)
	      show-user-list)
      (let ((rest-window
	     (split-window (selected-window)
			   (/ (* 5 (+ (window-height) command-height)) 8))))
	(cond
	 ;; channel-buffer + user-list
	 ((and show-user-list
	       (and riece-current-channel riece-channel-buffer-mode))
	  (let ((user-list-window (split-window (selected-window) nil t)))
	    (set-window-buffer (selected-window) riece-channel-buffer)
	    (set-window-buffer user-list-window riece-user-list-buffer)
	    (select-window user-list-window)
	    (shrink-window-horizontally (- (window-width) user-list-width))
	    (setq truncate-partial-width-windows nil)))
	 ;; only user-list
	 (show-user-list
	  (set-window-buffer (selected-window) riece-user-list-buffer))
	 ;; only channel-buffer
	 (riece-channel-buffer-mode
	  (set-window-buffer (selected-window) riece-channel-buffer)))
	(select-window rest-window)))
    ;; bottom of frame
    (if (and riece-current-channel
	     riece-channel-list-buffer-mode)
	(let ((channel-list-window (split-window (selected-window) nil t)))
	  (set-window-buffer (selected-window) riece-others-buffer)
	  (set-window-buffer channel-list-window riece-channel-list-buffer)
	  (select-window channel-list-window)
	  (shrink-window-horizontally (- (window-width) channel-list-width))
	  (setq truncate-partial-width-windows nil))
      (set-window-buffer (selected-window) riece-dialogue-buffer))
    (riece-set-window-points)
    (select-window (or (get-buffer-window buffer)
		       (get-buffer-window riece-command-buffer)))))

(provide 'riece-layout)

;;; riece-layout.el ends here
