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
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(require 'riece-globals)
(require 'riece-channel)
(require 'riece-user)
(require 'riece-display)

(defun riece-naming-assert-join (user-name channel-name)
  (riece-user-toggle-channel user-name channel-name t)
  (riece-channel-toggle-user channel-name user-name t)
  (let ((user-identity (riece-make-identity user-name
					    riece-server-name))
	(channel-identity (riece-make-identity channel-name
					       riece-server-name)))
    (when (riece-identity-equal-no-server user-name riece-real-nickname)
      (riece-join-channel channel-identity)
      (riece-switch-to-channel channel-identity)
      (setq riece-join-channel-candidate nil))
    (riece-emit-signal 'riece-naming-assert-join
		       user-identity channel-identity)))

(defun riece-naming-assert-part (user-name channel-name)
  (riece-user-toggle-channel user-name channel-name nil)
  (riece-channel-toggle-user channel-name user-name nil)
  (riece-channel-toggle-operator channel-name user-name nil)
  (riece-channel-toggle-speaker channel-name user-name nil)
  (let ((user-identity (riece-make-identity user-name
					    riece-server-name))
	(channel-identity (riece-make-identity channel-name
					       riece-server-name)))
    (if (riece-identity-equal-no-server user-name riece-real-nickname)
	(riece-part-channel channel-identity))
    (riece-emit-signal 'riece-naming-assert-part
		       user-identity channel-identity)))

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
  ;; Rename the channel buffer.
  (let* ((old-identity (riece-make-identity old-name riece-server-name))
	 (new-identity (riece-make-identity new-name riece-server-name))
	 (pointer (riece-identity-member old-identity riece-current-channels)))
    (when pointer
      (setcar pointer new-identity)
      (with-current-buffer (riece-channel-buffer old-identity)
	(rename-buffer (riece-channel-buffer-name new-identity) t)
	(setq riece-channel-buffer-alist
	      (cons (cons new-identity (current-buffer))
		    (delq (riece-identity-assoc old-identity
						riece-channel-buffer-alist)
			  riece-channel-buffer-alist))))
      (if (riece-identity-equal old-identity riece-current-channel)
	  (riece-switch-to-channel new-identity)))
    (riece-emit-signal 'riece-naming-assert-rename
		       old-identity new-identity)))

(defun riece-naming-assert-channel-users (users channel-name)
  (let ((channel-identity (riece-make-identity channel-name
					       riece-server-name))
	user-identity-list)
    (while users
      (riece-user-toggle-channel (car (car users)) channel-name t)
      (riece-channel-toggle-user channel-name (car (car users)) t)
      (if (memq ?o (cdr (car users)))
	  (riece-channel-toggle-operator channel-name (car (car users)) t)
	(if (memq ?v (cdr (car users)))
	    (riece-channel-toggle-speaker channel-name (car (car users)) t)
	  (riece-channel-toggle-operator channel-name (car (car users)) nil)
	  (riece-channel-toggle-speaker channel-name (car (car users)) nil)))
      (setq user-identity-list
	    (cons (cons (riece-make-identity (car (car users))
					     riece-server-name)
			(cdr (car users)))
		  user-identity-list))
      (when (riece-identity-equal-no-server (car (car users))
					    riece-real-nickname)
	(riece-join-channel channel-identity)
	(riece-switch-to-channel channel-identity)
	(setq riece-join-channel-candidate nil))
      (setq users (cdr users)))
    (riece-emit-signal 'riece-naming-assert-channel-users
		       (nreverse user-identity-list) channel-identity)))

(provide 'riece-naming)

;;; riece-naming.el ends here
