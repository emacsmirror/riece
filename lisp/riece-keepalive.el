;;; riece-keepalive.el --- keep an IRC connection
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
;; (add-to-list 'riece-addons 'riece-keepalive)

;;; Code:

(defvar riece-keepalive-ping-repeat 120)
(defvar riece-keepalive-timer nil)

(defun riece-keepalive-after-login-hook ()
  (make-local-variable 'riece-keepalive-timer)
  (unless riece-keepalive-timer
    (setq riece-keepalive-timer
	  (riece-run-at-time
	   riece-keepalive-ping-repeat riece-keepalive-ping-repeat
	   (lambda (buffer)
	     (save-excursion
	       (set-buffer buffer)
	       (riece-send-string "PING riece-keepalive\r\n")))
	   (current-buffer)))))

(defun riece-keepalive-after-close-hook ()
  (when riece-keepalive-timer
    (riece-cancel-timer riece-keepalive-timer)
    (setq riece-keepalive-timer nil)))

(defun riece-keepalive-insinuate ()
  (add-hook 'riece-after-login-hook #'riece-keepalive-after-login-hook)
  (add-hook 'riece-after-close-hook #'riece-keepalive-after-close-hook))

(provide 'riece-keepalive)

;;; riece-ignore.el ends here
