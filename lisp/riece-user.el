;;; riece-user.el --- a user object
;; Copyright (C) 1998-2003 Daiki Ueno

;; Author: Daiki Ueno <ueno@unixuser.org>
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

(eval-when-compile (require 'riece-inlines))	;string-assoc-ignore-case, etc.

(require 'riece-identity)

(defconst riece-user-regexp
  "[][\\\\`_^{|}A-Za-z][][\\\\`_^{|}A-Za-z0-9-]\\{0,8\\}")

;;; User object:
(defun riece-find-user (name)
  "Get a user object named NAME from the server buffer."
  (riece-with-server-buffer
   (let ((symbol (intern-soft (downcase (riece-identity-prefix name))
			      riece-obarray)))
     (if symbol
	 (symbol-value symbol)))))

(defun riece-forget-user (name)
  (riece-with-server-buffer
   (let ((symbol (intern-soft (downcase (riece-identity-prefix name)))))
     (when symbol
       (makunbound symbol)
       (unintern (symbol-name symbol) riece-obarray)))))

(defun riece-rename-user (old-name new-name)
  (riece-with-server-buffer
   (unless (equal (downcase (riece-identity-prefix old-name))
		  (downcase (riece-identity-prefix new-name)))
     (let ((symbol (intern-soft (downcase (riece-identity-prefix old-name))
				riece-obarray)))
       (when symbol
	 (set (intern (downcase (riece-identity-prefix new-name))
		      riece-obarray)
	      (symbol-value symbol))
	 (makunbound symbol)
	 (unintern (symbol-name symbol) riece-obarray))))))

(defun riece-make-user (&optional channels user-at-host modes away operator)
  "Make an instance of user object.
Arguments are appropriate to joined channels, user-at-host, mode, and
away status, respectively."
  (vector channels user-at-host modes away operator))

(defun riece-get-user (name)
  (riece-with-server-buffer
   (let ((symbol (intern-soft (downcase (riece-identity-prefix name))
			      riece-obarray)))
     (if symbol
	 (symbol-value symbol)
       (set (intern (downcase (riece-identity-prefix name)) riece-obarray)
	    (riece-make-user))))))

(defun riece-user-channels (user)
  "Return joined channels of USER."
  (aref user 0))

(defun riece-user-user-at-host (user)
  "Return the user-at-host of USER."
  (aref user 1))

(defun riece-user-modes (user)
  "Return the modes of USER."
  (aref user 2))

(defun riece-user-away (user)
  "Return t, if USER has been marked as away."
  (aref user 3))

(defun riece-user-operator (user)
  "Return t, if USER has operator privilege."
  (aref user 4))

(defun riece-user-set-channels (user value)
  "Set the joined channels of USER to VALUE."
  (aset user 0 value))

(defun riece-user-set-user-at-host (user value)
  "Set the user-at-host of USER to VALUE."
  (aset user 1 value))

(defun riece-user-set-modes (user value)
  "Set the modes of USER to VALUE."
  (aset user 2 value))

(defun riece-user-set-away (user value)
  "Set the away status of USER to VALUE."
  (aset user 3 value))

(defun riece-user-set-operator (user value)
  "Set the operator status of USER to VALUE."
  (aset user 4 value))

(defun riece-user-get-channels (&optional name)
  (riece-user-channels
   (riece-get-user (or name riece-real-nickname))))

(defun riece-user-get-user-at-host (&optional name)
  (riece-user-user-at-host
   (riece-get-user (or name riece-real-nickname))))

(defun riece-user-get-modes (&optional name)
  (riece-user-modes
   (riece-get-user (or name riece-real-nickname))))

(defun riece-user-get-away (&optional name)
  (riece-user-away
   (riece-get-user (or name riece-real-nickname))))

(defun riece-user-get-operator (&optional name)
  (riece-user-operator
   (riece-get-user (or name riece-real-nickname))))

(defun riece-user-toggle-channel (name channel flag)
  "Add or remove the joined channel of user."
  (let* ((user (riece-get-user (or name (riece-current-nickname))))
	 (channels (riece-user-channels user)))
    (if flag
	(unless (member channel channels)
	  (riece-user-set-channels user (cons channel channels)))
      (if (setq channel (car (member channel channels)))
	  (riece-user-set-channels user (delq channel channels))))))

(defun riece-user-toggle-mode (name mode flag)
  "Add or remove user MODE of user."
  (let* ((user (riece-get-user (or name (riece-current-nickname))))
	 (modes (riece-user-modes user)))
    (if flag
	(unless (memq mode modes)
	  (riece-user-set-modes user (cons mode modes)))
      (if (memq mode modes)
	  (riece-user-set-modes user (delq mode modes))))))

(defun riece-user-toggle-away (name flag)
  (riece-user-set-away
   (riece-get-user (or name (riece-current-nickname))) flag))

(defun riece-user-toggle-operator (name flag)
  (riece-user-set-operator
   (riece-get-user (or name (riece-current-nickname))) flag))

(provide 'riece-user)

;;; riece-user.el ends here
