;;; riece-lsdb.el --- interface to LSDB
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
;; (add-to-list 'riece-addons 'riece-lsdb)

;;; Code:

(eval-when-compile
  (autoload 'lsdb-maybe-load-hash-tables "lsdb")
  (autoload 'lsdb-lookup-records "lsdb")
  (autoload 'lsdb-puthash "lsdb")
  (autoload 'lsdb-remhash "lsdb")
  (autoload 'lsdb-gethash "lsdb")
  (autoload 'lsdb-display-records "lsdb"))

(defvar riece-lsdb-cache nil)

(defun riece-lsdb-update-cache (record)
  (let ((irc (cdr (assq 'irc record))))
    (while irc
      (lsdb-puthash (car irc) (car record) riece-lsdb-cache)
      (setq irc (cdr irc)))))

(defun riece-lsdb-delete-cache (record)
  (let ((irc (cdr (assq 'irc record))))
    (while irc
      (lsdb-remhash (car irc) riece-lsdb-cache)
      (setq irc (cdr irc)))))

(defun riece-lsdb-lookup-records (user)
  (lsdb-maybe-load-hash-tables)
  (let ((name (lsdb-gethash (riece-format-identity user t)
			    riece-lsdb-cache)))
    (if name
	(lsdb-lookup-records name))))

(defun riece-lsdb-display-records (user)
  (interactive
   (let ((completion-ignore-case t))
     (list (riece-completing-read-identity
	    "User: "
	    (riece-get-users-on-server (riece-current-server-name))))))
  (let ((records (riece-lsdb-lookup-records user)))
    (if records
	(lsdb-display-records records)
      (message "No entry for `%s'" (riece-format-identity user t)))))

(defun riece-lsdb-insinuate ()
  (require 'lsdb)
  (add-to-list 'lsdb-secondary-hash-tables
	       'riece-lsdb-cache)
  (add-to-list 'lsdb-after-update-record-functions
	       'riece-lsdb-update-cache)
  (add-to-list 'lsdb-after-delete-record-functions
	       'riece-lsdb-delete-cache)
  (define-key riece-command-mode-map
    "\C-cL" 'riece-lsdb-display-records))

(provide 'riece-lsdb)

;;; riece-lsdb.el ends here
