;;; riece-400.el --- handlers for 400 replies
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
(require 'riece-misc)

(eval-when-compile
  (autoload 'riece-default-handle-numeric-reply "riece-filter"))
(defun riece-handle-default-400-message (prefix number name string)
  (riece-default-handle-numeric-reply
   riece-error-prefix prefix number name string))

(defun riece-handle-read-string (prompt)
  (condition-case nil
      (let (inhibit-quit)
	(read-string prompt))
    (quit
     (ignore (message "%s" (concat prompt "Quit"))))))

(defun riece-handle-432-message (prefix number name string)
  "ERR_ERRONEUSNICKNAME	\"<nick> :Erroneous nickname\"."
  (let ((nickname
	 (riece-handle-read-string
	  (format "Erroneous nickname \"%s\".  Choose a new one: "
		  (car (riece-split-parameters string))))))
    (if nickname
	(riece-send-string (format "NICK %s\r\n" nickname)))))

(defun riece-handle-433-message (prefix number name string)
  "ERR_NICKNAMEINUSE \"<nick> :Nickname is already in use\"."
  (let ((nickname
	 (riece-handle-read-string
	  (format "Nickname \"%s\" already in use.  Choose a new one: "
		  (car (riece-split-parameters string))))))
    (if nickname
	(riece-send-string (format "NICK %s\r\n" nickname)))))

(defun riece-handle-464-message (prefix number name string)
  "ERR_PASSWDMISMATCH \":Password incorrect\"."
  (message "Password incorrect from %s." prefix)
  (setq riece-reconnect-with-password t))

(provide 'riece-400)

;;; riece-400.el ends here
