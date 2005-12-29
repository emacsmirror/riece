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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'riece-identity)
(require 'riece-mode)
(require 'riece-cache)

;;; User object:
(defun riece-find-user (name)
  "Get a user object named NAME from the server buffer."
  (riece-cache-get riece-user-cache name)
  (let ((symbol (intern-soft (riece-identity-canonicalize-prefix name)
			     riece-user-obarray)))
    (if symbol
	(symbol-value symbol))))

(defun riece-forget-user (name)
  (riece-cache-delete riece-user-cache name)
  (let ((symbol (intern-soft (riece-identity-canonicalize-prefix name))))
    (when symbol
      (makunbound symbol)
      (unintern (symbol-name symbol) riece-user-obarray))))

(defun riece-rename-user (old-name new-name)
  (riece-cache-delete riece-user-cache old-name)
  (riece-cache-set riece-user-cache new-name new-name)
  (unless (equal (riece-identity-canonicalize-prefix old-name)
		 (riece-identity-canonicalize-prefix new-name))
    (let ((symbol (intern-soft (riece-identity-canonicalize-prefix old-name)
			       riece-user-obarray)))
      (when symbol
	(set (intern (riece-identity-canonicalize-prefix new-name)
		     riece-user-obarray)
	     (symbol-value symbol))
	(makunbound symbol)
	(unintern (symbol-name symbol) riece-user-obarray)))))

(defun riece-make-user (channels user-at-host modes away operator)
  "Make an instance of user object.
Arguments are appropriate to joined channels, user-at-host, mode, and
away status, respectively."
  (vector channels user-at-host modes away operator))

(defun riece-get-user (name)
  (let ((symbol (intern-soft (riece-identity-canonicalize-prefix name)
			     riece-user-obarray)))
     (if symbol
	 (progn
	   (riece-cache-get riece-user-cache name)
	   (symbol-value symbol))
       (riece-cache-set riece-user-cache name name)
       (set (intern (riece-identity-canonicalize-prefix name)
		    riece-user-obarray)
	    (riece-make-user nil nil nil nil nil)))))

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

(defun riece-user-get-channels (name)
  (riece-user-channels (riece-get-user name)))

(defun riece-user-get-user-at-host (name)
  (riece-user-user-at-host (riece-get-user name)))

(defun riece-user-get-modes (name)
  (riece-user-modes (riece-get-user name)))

(defun riece-user-get-away (name)
  (riece-user-away (riece-get-user name)))

(defun riece-user-get-operator (name)
  (riece-user-operator (riece-get-user name)))

(defun riece-user-toggle-channel (name channel flag)
  "Add or remove the joined channel of user."
  (let* ((user (riece-get-user name))
	 (channels (riece-user-channels user)))
    (if flag
	(unless (member channel channels)
	  (riece-user-set-channels user (cons channel channels)))
      (if (setq channel (car (member channel channels)))
	  (riece-user-set-channels user (delq channel channels))))))

(defun riece-user-toggle-mode (name mode flag)
  "Add or remove user MODE of user."
  (let* ((user (riece-get-user name))
	 (modes (riece-user-modes user))
	 (old (riece-mode-assoc (riece-mode-flag mode) modes)))
    (if flag
	(unless old
	  (riece-user-set-modes user (cons mode modes)))
      (if old
	  (riece-user-set-modes user (delq mode modes))))))

(defun riece-user-toggle-away (name flag)
  (riece-user-set-away (riece-get-user name) flag))

(defun riece-user-toggle-operator (name flag)
  (riece-user-set-operator (riece-get-user name) flag))

(provide 'riece-user)

;;; riece-user.el ends here
