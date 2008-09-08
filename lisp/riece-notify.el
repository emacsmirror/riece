;;; riece-notify.el --- display notification on status area
;; Copyright (C) 1998-2008 Daiki Ueno

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

(require 'riece-message)
(eval-when-compile (require 'riece-keyword))
(require 'dbus)

(defconst riece-notify-description "Display notification on status area.")

(defun riece-notify-keyword-notify-function (keyword message)
  (riece--notify (format "%s: %s"
			 (riece-format-identity (riece-message-speaker message))
			 (riece-message-text message))))

(defun riece--notify (string)
  (dbus-call-method
   :session "org.freedesktop.Notifications"
   "/org/freedesktop/Notifications"
   "org.freedesktop.Notifications" "Notify"
   "GNU Emacs"                 ;; Application name.
   0                           ;; No replacement of other notifications.
   ""                          ;; No icon.
   "Notification summary"      ;; Summary.
   (encode-coding-string string 'utf-8) ;; Body.
   '(:array)                   ;; No actions (empty array of strings).
   '(:array :signature "{sv}") ;; No hints
   ;; (empty array of dictionary entries).
   ':int32 -1)                 ;; Default timeout.
  )

(defun riece-notify-requires ()
  '(riece-keyword))

(defun riece-notify-insinuate ()
  (add-hook 'riece-keyword-notify-functions
	    'riece-notify-keyword-notify-function))

(defun riece-notify-uninstall ()
  (remove-hook 'riece-keyword-notify-functions
	       'riece-notify-keyword-notify-function))

(provide 'riece-notify)

;;; riece-notify.el ends here
