;;; riece-xface.el --- display X-Face in user list buffer
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

;;; Commentary:

;; To use, add the following line to your ~/.riece/init.el:
;; (add-to-list 'riece-addons 'riece-xface)

;;; Code:

(require 'riece-identity)
(require 'riece-globals)
(require 'riece-display)
(require 'riece-lsdb)

(defvar riece-xface-enabled nil)

(defconst riece-xface-description
  "Display X-Face in user list buffer")

(defvar lsdb-insert-x-face-function)

(defun riece-xface-update-user-list-buffer ()
  (if riece-xface-enabled
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

(defun riece-xface-insinuate ()
  (add-hook 'riece-user-list-mode-hook
	    (lambda ()
	      (add-hook 'riece-update-buffer-functions
			'riece-xface-update-user-list-buffer t t))))

(defun riece-xface-enable ()
  (setq riece-xface-enabled t)
  (if riece-current-channel
      (riece-emit-signal 'user-list-changed riece-current-channel)))

(defun riece-xface-disable ()
  (setq riece-xface-enabled nil)
  (if riece-current-channel
      (riece-emit-signal 'user-list-changed riece-current-channel)))

(provide 'riece-xface)

;;; riece-xface.el ends here
