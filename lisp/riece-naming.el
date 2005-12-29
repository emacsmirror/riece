;;; riece-naming.el --- toplevel naming management
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

;;; Code:

(require 'riece-globals)
(require 'riece-channel)
(require 'riece-user)
(require 'riece-signal)

(defun riece-naming-assert-join (user-name channel-name)
  (riece-user-toggle-channel user-name channel-name t)
  (riece-channel-toggle-user channel-name user-name t)
  (riece-emit-signal 'user-joined-channel
		     (riece-make-identity user-name
					  riece-server-name)
		     (riece-make-identity channel-name
					  riece-server-name)))

(defun riece-naming-assert-part (user-name channel-name)
  (riece-user-toggle-channel user-name channel-name nil)
  (riece-channel-toggle-user channel-name user-name nil)
  (riece-channel-toggle-operator channel-name user-name nil)
  (riece-channel-toggle-speaker channel-name user-name nil)
  (riece-emit-signal 'user-left-channel
		     (riece-make-identity user-name
					  riece-server-name)
		     (riece-make-identity channel-name
					  riece-server-name)))

(defun riece-naming-assert-rename (old-name new-name)
  (if (riece-identity-equal-no-server old-name riece-real-nickname)
      (setq riece-last-nickname riece-real-nickname
	    riece-real-nickname new-name))
  (let* ((old (riece-get-user old-name))
	 (channels (riece-user-channels old))
	 users user)
    (while channels
      (setq users (riece-channel-get-users (car channels))
	    user (riece-identity-assoc old-name users t))
      (if user
	  (setcar user new-name))
      (setq channels (cdr channels)))
    (riece-rename-user old-name new-name))
  (riece-emit-signal 'user-renamed
		     (riece-make-identity old-name riece-server-name)
		     (riece-make-identity new-name riece-server-name)))

(defun riece-naming-assert-channel-users (users channel-name)
  (let ((channel-identity (riece-make-identity channel-name
					       riece-server-name))
	(pointer users))
    (while pointer
      (riece-user-toggle-channel (car (car pointer)) channel-name t)
      (riece-channel-toggle-user channel-name (car (car pointer)) t)
      (if (memq ?o (cdr (car pointer)))
	  (riece-channel-toggle-operator channel-name (car (car pointer)) t)
	(if (memq ?v (cdr (car pointer)))
	    (riece-channel-toggle-speaker channel-name (car (car pointer)) t)
	  (riece-channel-toggle-operator channel-name (car (car pointer)) nil)
	  (riece-channel-toggle-speaker channel-name (car (car pointer)) nil)))
      (setq pointer (cdr pointer)))
    ;; Remove nonexistent users.
    (setq pointer (riece-channel-users (riece-get-channel channel-name)))
    (while pointer
      (unless (assoc (car (car pointer)) users)
	(riece-user-toggle-channel (car (car pointer)) channel-name nil)
	(riece-channel-toggle-user channel-name (car (car pointer)) nil))
      (setq pointer (cdr pointer)))
    (riece-emit-signal 'user-list-changed channel-identity)))

(defun riece-naming-assert-channel-modes (channel modes)
  (while modes
    (cond
     ((eq (riece-mode-flag (car (car modes))) ?o)
      (riece-channel-toggle-operator channel
				     (riece-mode-parameter (car (car modes)))
				     (nth 1 (car modes)))
      (riece-emit-signal 'channel-operators-changed
			 (riece-make-identity channel
					      riece-server-name)
			 (riece-make-identity (riece-mode-parameter
					       (car (car modes)))
					      riece-server-name)
			 (nth 1 (car modes))))
     ((eq (riece-mode-flag (car (car modes))) ?v)
      (riece-channel-toggle-speaker channel
				    (riece-mode-parameter (car (car modes)))
				    (nth 1 (car modes)))
      (riece-emit-signal 'channel-speakers-changed
			 (riece-make-identity channel
					      riece-server-name)
			 (riece-make-identity (riece-mode-parameter
					       (car (car modes)))
					      riece-server-name)
			 (nth 1 (car modes))))
     ((eq (riece-mode-flag (car (car modes))) ?b)
      (riece-channel-toggle-banned channel
				   (riece-mode-parameter (car (car modes)))
				   (nth 1 (car modes))))
     ((eq (riece-mode-flag (car (car modes))) ?e)
      (riece-channel-toggle-uninvited channel
				      (riece-mode-parameter (car (car modes)))
				      (nth 1 (car modes))))
     ((eq (riece-mode-flag (car (car modes))) ?I)
      (riece-channel-toggle-invited channel
				    (riece-mode-parameter (car (car modes)))
				    (nth 1 (car modes))))
     (t
      (apply #'riece-channel-toggle-mode channel (car modes))))
    (setq modes (cdr modes)))
  (riece-emit-signal 'channel-modes-changed
		     (riece-make-identity channel riece-server-name)))

(provide 'riece-naming)

;;; riece-naming.el ends here
