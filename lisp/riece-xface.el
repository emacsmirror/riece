;;; riece-xface.el --- display X-Face in IRC buffers
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

;;; Commentary:

;; NOTE: This is an add-on module for Riece.

;;; Code:

(require 'riece-identity)
(require 'riece-globals)
(require 'riece-display)
(require 'riece-lsdb)

(defconst riece-xface-description
  "Display X-Face in IRC buffers.")

(defvar lsdb-insert-x-face-function)

(defun riece-xface-update-user-list-buffer ()
  (if (get 'riece-xface 'riece-addon-enabled)
      (riece-scan-property-region
       'riece-identity (point-min)(point-max)
       (lambda (start end)
	 (let ((records (riece-lsdb-lookup-records (get-text-property
						    start 'riece-identity)))
	       xface)
	   (while (and records
		       (null xface))
	     (setq xface (nth 1 (assq 'x-face (car records)))
		   records (cdr records)))
	   (if (and xface
		    (not (eq (char-after end) ? )))
	       (let ((inhibit-read-only t)
		     buffer-read-only)
		 (goto-char end)
		 (insert " ")
		 (funcall lsdb-insert-x-face-function xface))))))))

(defun riece-xface-requires ()
  '(riece-lsdb))

(defun riece-xface-user-list-mode-hook ()
  (add-hook 'riece-update-buffer-functions
	    'riece-xface-update-user-list-buffer t t))

(defun riece-xface-insinuate ()
  (if riece-user-list-buffer
      (with-current-buffer riece-user-list-buffer
	(riece-xface-user-list-mode-hook)))
  (add-hook 'riece-user-list-mode-hook
	    'riece-xface-user-list-mode-hook))

(defun riece-xface-uninstall ()
  (if riece-user-list-buffer
      (with-current-buffer riece-user-list-buffer
	(remove-hook 'riece-update-buffer-functions
		     'riece-xface-update-user-list-buffer t)))
  (remove-hook 'riece-user-list-mode-hook
	       'riece-xface-user-list-mode-hook))

(defun riece-xface-enable ()
  (if riece-current-channel
      (riece-emit-signal 'user-list-changed riece-current-channel)))

(defun riece-xface-disable ()
  (if riece-current-channel
      (riece-emit-signal 'user-list-changed riece-current-channel)))

(provide 'riece-xface)

;;; riece-xface.el ends here
