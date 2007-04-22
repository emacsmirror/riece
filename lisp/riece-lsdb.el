;;; riece-lsdb.el --- help register nicknames in LSDB rolodex program
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
(require 'riece-misc)

(eval-when-compile
  (autoload 'lsdb-maybe-load-hash-tables "lsdb")
  (autoload 'lsdb-rebuild-secondary-hash-tables "lsdb")
  (autoload 'lsdb-lookup-records "lsdb")
  (autoload 'lsdb-puthash "lsdb")
  (autoload 'lsdb-maphash "lsdb")
  (autoload 'lsdb-gethash "lsdb")
  (autoload 'lsdb-display-records "lsdb")
  (autoload 'lsdb-update-record "lsdb"))

(defvar riece-lsdb-cache nil)

(defconst riece-lsdb-description
  "Help register nicknames in LSDB rolodex program.")

(defun riece-lsdb-update-cache (record)
  (let ((irc (cdr (assq 'irc record))))
    (while irc
      (lsdb-puthash (car irc)
		    (cons (car record)
			  (lsdb-gethash (car irc) riece-lsdb-cache))
		    riece-lsdb-cache)
      (setq irc (cdr irc)))))

(defun riece-lsdb-delete-cache (record)
  (let ((irc (cdr (assq 'irc record))))
    (while irc
      (lsdb-puthash (car irc)
		    (delete (car record)
			    (lsdb-gethash (car irc) riece-lsdb-cache))
		    riece-lsdb-cache)
      (setq irc (cdr irc)))))

(defun riece-lsdb-lookup-records (user)
  (lsdb-maybe-load-hash-tables)
  (unless riece-lsdb-cache
    (lsdb-rebuild-secondary-hash-tables))
  (let ((names (lsdb-gethash (riece-format-identity user t)
			     riece-lsdb-cache))
	records)
    (while names
      (setq records (append records (lsdb-lookup-records (car names))))
      (setq names (cdr names)))
    records))

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

(defvar lsdb-hash-table)
(defun riece-lsdb-add-user (user full-name)
  (interactive
   (let ((completion-ignore-case t)
	 (table lsdb-hash-table))
     (unless (vectorp table)
       (setq table (make-vector 29 0))
       (lsdb-maphash (lambda (key value)
		       (intern key table))
		     lsdb-hash-table))
     (list (riece-completing-read-identity
	    "User: "
	    (riece-get-users-on-server (riece-current-server-name)))
	   (completing-read "Full name: " table))))
  (let* ((record (lsdb-gethash full-name lsdb-hash-table))
	 (irc (riece-format-identity user t))
	 (old (cdr (assq 'irc record))))
    ;; Remove all properties before adding entry.
    (set-text-properties 0 (length irc) nil irc)
    (unless (member irc old)
      (lsdb-update-record (list full-name
				;; LSDB does not allow empty 'net entry.
				(or (nth 1 (assq 'net (lsdb-lookup-records
						       full-name)))
				    ""))
			  (list (cons 'irc (cons irc old)))))))

(defvar riece-command-mode-map)
(defvar lsdb-secondary-hash-tables)
(defvar lsdb-after-update-record-functions)
(defvar lsdb-after-delete-record-functions)
(defun riece-lsdb-insinuate ()
  (require 'lsdb)
  (add-to-list 'lsdb-secondary-hash-tables
	       'riece-lsdb-cache)
  (add-to-list 'lsdb-after-update-record-functions
	       'riece-lsdb-update-cache)
  (add-to-list 'lsdb-after-delete-record-functions
	       'riece-lsdb-delete-cache))

(defun riece-lsdb-uninstall ()
  (setq lsdb-secondary-hash-tables
	(delq 'riece-lsdb-cache lsdb-secondary-hash-tables)
	lsdb-after-update-record-functions
	(delq 'riece-lsdb-update-cache lsdb-after-update-record-functions)
	lsdb-after-delete-record-functions
	(delq 'riece-lsdb-delete-cache lsdb-after-delete-record-functions)))

(defun riece-lsdb-enable ()
  (define-key riece-command-mode-map
    "\C-c\C-ll" 'riece-lsdb-display-records)
  (define-key riece-command-mode-map
    "\C-c\C-la" 'riece-lsdb-add-user))

(defun riece-lsdb-disable ()
  (define-key riece-command-mode-map
    "\C-c\C-ll" nil)
  (define-key riece-command-mode-map
    "\C-c\C-la" nil))

(provide 'riece-lsdb)

;;; riece-lsdb.el ends here
