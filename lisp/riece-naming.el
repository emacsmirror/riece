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
  (if (riece-identity-equal-no-server user-name riece-real-nickname)
      (let ((channel-identity (riece-make-identity channel-name
						   riece-server-name)))
	(riece-join-channel channel-identity)
	(riece-switch-to-channel channel-identity))
    (if (and riece-current-channel
	     (riece-identity-equal (riece-make-identity channel-name
							riece-server-name)
				   riece-current-channel))
	(with-current-buffer riece-user-list-buffer
	  (setq riece-redisplay-buffer t)))))

(defun riece-naming-assert-part (user-name channel-name)
  (riece-user-toggle-channel user-name channel-name nil)
  (riece-channel-toggle-user channel-name user-name nil)
  (riece-channel-toggle-operator channel-name user-name nil)
  (riece-channel-toggle-speaker channel-name user-name nil)
  (if (riece-identity-equal-no-server user-name riece-real-nickname)
      (riece-part-channel (riece-make-identity channel-name
					       riece-server-name))
    (if (and riece-current-channel
	     (riece-identity-equal (riece-make-identity channel-name
							riece-server-name)
				   riece-current-channel))
	(with-current-buffer riece-user-list-buffer
	  (setq riece-redisplay-buffer t)))))

(defun riece-naming-assert-rename (old-name new-name)
  (if (riece-identity-equal-no-server old-name riece-real-nickname)
      (setq riece-last-nickname riece-real-nickname
	    riece-real-nickname new-name))
  (let* ((old (riece-get-user old-name))
	 (channels (riece-user-channels old))
	 users pointer)
    (while channels
      (setq users (riece-channel-get-users (car channels))
	    pointer (member old-name users))
      (if pointer
	  (setcar pointer new-name))
      (setq users (riece-channel-get-operators (car channels))
	    pointer (member old-name users))
      (if pointer
	  (setcar pointer new-name))
      (setq users (riece-channel-get-speakers (car channels))
	    pointer (member old-name users))
      (if pointer
	  (setcar pointer new-name))
      (if (and riece-current-channel
	       (riece-identity-equal (riece-make-identity (car channels)
							  riece-server-name)
				     riece-current-channel))
	  (with-current-buffer riece-user-list-buffer
	    (setq riece-redisplay-buffer t)))
      (setq channels (cdr channels)))
    (riece-rename-user old-name new-name)))

(provide 'riece-naming)

;;; riece-naming.el ends here
