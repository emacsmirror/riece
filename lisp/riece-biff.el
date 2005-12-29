;;; riece-biff.el --- be notified if messages arrives
;; Copyright (C) 2004 OHASHI Akira

;; Author: OHASHI Akira <bg66@koka-in.org>
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

(require 'riece-message)

(defgroup riece-biff nil
  "Be notified if messages arrives."
  :prefix "riece-"
  :group 'riece)

(defcustom riece-biff-check-channels nil
  "*If non-nil, riece-biff checks only the channel contained in this list."
  :type '(repeat string)
  :group 'riece-biff)

(defcustom riece-biff-default-mode-string "[-]"
  "*String displayed when there is no arrival message."
  :type 'string
  :group 'riece-biff)

(defcustom riece-biff-biff-mode-string "[R]"
  "*String displayed when there are new arrival messages."
  :type 'string
  :group 'riece-biff)

(defvar riece-biff-mode-string 'riece-biff-default-mode-string)

(defconst riece-biff-description
  "Be notified if messages arrives.")

(defun riece-biff-after-display-message-function (message)
  (when (and (get 'riece-biff 'riece-addon-enabled)
	     (not (or (eq (window-buffer (selected-window))
			  (get-buffer riece-command-buffer))
		      (riece-message-own-p message)
		      (riece-message-type message))))
    (when (or (null riece-biff-check-channels)
	      (member (riece-format-identity (riece-message-target message))
		      riece-biff-check-channels))
      (setq riece-biff-mode-string 'riece-biff-biff-mode-string))))

(defun riece-biff-clear (&optional dummy)
  (when (get 'riece-biff 'riece-addon-enabled)
    (setq riece-biff-mode-string 'riece-biff-default-mode-string)))

(defun riece-biff-insinuate ()
  (add-hook 'riece-after-display-message-functions
	    'riece-biff-after-display-message-function)
  (add-hook 'riece-redisplay-buffers-hook 'riece-biff-clear)
  (add-hook 'riece-after-switch-to-channel-functions 'riece-biff-clear)
  (add-hook 'riece-exit-hook 'riece-biff-disable))

(defun riece-biff-uninstall ()
  (remove-hook 'riece-after-display-message-functions
	       'riece-biff-after-display-message-function)
  (remove-hook 'riece-redisplay-buffers-hook 'riece-biff-clear)
  (remove-hook 'riece-after-switch-to-channel-functions 'riece-biff-clear)
  (remove-hook 'riece-exit-hook 'riece-biff-disable))

(defun riece-biff-enable ()
  (setq global-mode-string
	(cond
	 ((nlistp global-mode-string)
	  (list "" 'riece-biff-mode-string global-mode-string))
	 ((not (memq 'riece-biff-mode-string global-mode-string))
	  (append '("" riece-biff-mode-string)
		  (remove "" global-mode-string)))
	 (t
	  global-mode-string))))

(defun riece-biff-disable ()
  (setq global-mode-string
	(cond
	 ((and (listp global-mode-string)
	       (memq 'riece-biff-mode-string global-mode-string))
	  (remq 'riece-biff-mode-string global-mode-string))
	 (t
	  global-mode-string)))
  (riece-biff-clear))

(provide 'riece-biff)

;;; riece-biff.el ends here
