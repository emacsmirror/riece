;;; riece-shrink-buffer.el --- free old IRC messages to save memory usage
;; Copyright (C) 1998-2005 Daiki Ueno

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

(require 'riece-globals)
(require 'derived)

(defgroup riece-shrink-buffer nil
  "Free old IRC messages to save memory usage."
  :prefix "riece-"
  :group 'riece)
  
(defcustom riece-shrink-buffer-idle-time-delay 5
  "Number of idle seconds to wait before shrinking channel buffers."
  :type 'integer
  :group 'riece-shrink-buffer)

(defcustom riece-max-buffer-size 65536
  "Maximum size of channel buffers."
  :type '(integer :tag "Number of characters")
  :group 'riece-shrink-buffer)

(defcustom riece-shrink-buffer-remove-chars (/ riece-max-buffer-size 2)
  "Number of chars removed when shrinking channel buffers."
  :type 'integer
  :group 'riece-shrink-buffer)

(defvar riece-shrink-buffer-idle-timer nil
  "Timer object to periodically shrink channel buffers.")

(defconst riece-shrink-buffer-description
  "Free old IRC messages to save memory usage.")

(defun riece-shrink-buffer-idle-timer ()
  (let ((buffers riece-buffer-list))
    (while buffers
      (if (and (get 'riece-shrink-buffer 'riece-addon-enabled)
	       (buffer-live-p (car buffers))
	       (with-current-buffer (car buffers)
		 (riece-derived-mode-p 'riece-dialogue-mode)))
	  (riece-shrink-buffer (car buffers)))
      (setq buffers (cdr buffers)))))

(defun riece-shrink-buffer (buffer)
  (save-excursion
    (set-buffer buffer)
    (goto-char (point-min))
    (while (> (buffer-size) riece-max-buffer-size)
      (let* ((inhibit-read-only t)
	     buffer-read-only
	     (end (progn
		    (goto-char riece-shrink-buffer-remove-chars)
		    (beginning-of-line 2)
		    (point)))
	     (overlays (riece-overlays-in (point-min) end)))
	(while overlays
	  (riece-delete-overlay (car overlays))
	  (setq overlays (cdr overlays)))
	(delete-region (point-min) end)))))

(defun riece-shrink-buffer-startup-hook ()
  (setq riece-shrink-buffer-idle-timer
	(riece-run-with-idle-timer
	 riece-shrink-buffer-idle-time-delay t
	 'riece-shrink-buffer-idle-timer)))

(defun riece-shrink-buffer-exit-hook ()
  (if riece-shrink-buffer-idle-timer
      (riece-cancel-timer riece-shrink-buffer-idle-timer)))

(defun riece-shrink-buffer-insinuate ()
  (add-hook 'riece-startup-hook
	    'riece-shrink-buffer-startup-hook)
  ;; Reset the timer since riece-shrink-buffer-insinuate will be
  ;; called before running riece-startup-hook.
  (unless riece-shrink-buffer-idle-timer
    (riece-shrink-buffer-startup-hook))
  (add-hook 'riece-exit-hook
	    'riece-shrink-buffer-exit-hook))

(defun riece-shrink-buffer-uninstall ()
  (riece-shrink-buffer-exit-hook)
  (remove-hook 'riece-startup-hook
	       'riece-shrink-buffer-startup-hook)
  (remove-hook 'riece-exit-hook
	       'riece-shrink-buffer-exit-hook))

(provide 'riece-shrink-buffer)

;;; riece-shrink-buffer.el ends here
