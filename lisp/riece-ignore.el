;;; riece-ignore.el --- ignore user
;; Copyright (C) 1998-2004 Daiki Ueno

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
;; (add-to-list 'riece-addons 'riece-ignore)

;;; Code:

(require 'riece-signal)
(require 'riece-identity)
(require 'riece-message)

(defvar riece-ignored-user nil)

(defun riece-ignore-by-user (user)
  (interactive
   (let ((completion-ignore-case t))
     (list (riece-completing-read-identity
	    "User: "
	    (riece-get-users-on-server (riece-current-server-name))))))
  (setq riece-ignored-user (cons user riece-ignored-user))
  (riece-connect-signal
   'user-renamed
   (lambda (signal handback)
     (let ((pointer (riece-identity-member (car (riece-signal-args signal))
					   riece-ignored-user)))
       (if pointer
	   (setcar pointer (nth 1 (riece-signal-args signal))))))))

(defun riece-ignore-message-filter (message)
  (unless (riece-identity-member (riece-message-speaker message)
				 riece-ignored-user)
    message))

(defvar riece-command-mode-map)
(defun riece-ignore-insinuate ()
  (add-hook 'riece-message-filter-functions 'riece-ignore-message-filter)
  (define-key riece-command-mode-map
    "\C-ck" 'riece-ignore-by-user))

(provide 'riece-ignore)

;;; riece-ignore.el ends here