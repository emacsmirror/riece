;;; riece-log.el --- saving irc logs add-on
;; Copyright (C) 2003 OHASHI Akira
;; Copyright (C) 2004 Daiki Ueno

;; Author: OHASHI Akira <bg66@koka-in.org>
;;	Daiki Ueno <ueno@unixuser.org>
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

;; This add-on saves irc logs for every channel.

;; To use, add the following line to your ~/.riece/init.el:
;; (add-to-list 'riece-addons 'riece-log)

;;; Code:

(require 'riece-message)
(require 'riece-button)

(defgroup riece-log nil
  "Save irc log"
  :group 'riece)

(defcustom riece-log-directory
  (expand-file-name "log" riece-directory)
  "*Where to look for log files."
  :type 'directory
  :group 'riece-log)

(defcustom riece-log-lock-directory
  (expand-file-name "=lock" riece-log-directory)
  "*Lock directory for riece-log.
It is created if there is at least one instance of Emacs running riece-log."
  :type 'directory
  :group 'riece-log)

(defcustom riece-log-directory-map nil
  "*The map of channel name and directory name."
  :type '(repeat (cons (string :tag "Channel name")
		       (string :tag "Directory name")))
  :group 'riece-log)

(defcustom riece-log-flashback 10
  "*If non-nil, irc messages flash back from log files.
If integer, flash back only this line numbers. t means all lines."
  :type '(choice (integer :tag "line numbers")
		 (const t :tag "of the day")
		 (const nil :tag "no flashback"))
  :group 'riece-log)

(defcustom riece-log-coding-system nil
  "*Coding system used for log files."
  :type 'symbol
  :group 'riece-log)

(defcustom riece-log-file-name-coding-system
  (if (boundp 'file-name-coding-system)
      file-name-coding-system)
  "*Coding system used for filenames of log files."
  :type 'symbol
  :group 'riece-log)

(defcustom riece-log-open-directory-function 'find-file
  "*Function for opening a directory."
  :type 'function
  :group 'riece-log)

(defface riece-log-date-face
  '((((class color)
      (background dark))
     (:foreground "Gray70"))
    (((class color)
      (background light))
     (:foreground "DimGray"))
    (t
     (:bold t)))
  "Face used for displaying \"(YYYY/MM/dd)\" extent."
  :group 'riece-highlight-faces)
(defvar riece-log-date-face 'riece-log-date-face)

(defvar riece-log-enabled nil)

(defconst riece-log-description
  "Saving IRC logs")

(defun riece-log-display-message-function (message)
  (if riece-log-enabled
      (let ((file (riece-log-get-file (riece-message-target message)))
	    (coding-system-for-write riece-log-coding-system)
	    file-name-coding-system
	    default-file-name-coding-system)
	(unless (file-directory-p (file-name-directory file))
	  (make-directory (file-name-directory file) t))
	(write-region (concat (format-time-string "%H:%M") " "
			      (riece-format-message message))
		      nil file t 0))))

(defun riece-log-get-file (identity)
  (expand-file-name
   (concat (format-time-string "%Y%m%d") ".txt")
   (riece-log-get-directory identity)))

(defun riece-log-get-files (identity)
  (let ((directory (riece-log-get-directory identity)))
    (if (file-directory-p directory)
	(nreverse (sort (directory-files directory t
			 (concat "^"
				 (riece-make-interval-regexp "[0-9]" 8)
				 "\\.txt$")
			 t)
		  #'string-lessp)))))

(defun riece-log-get-directory (identity)
  (let ((prefix (riece-identity-canonicalize-prefix
		 (riece-identity-prefix identity)))
	(server (riece-identity-server identity))
	(map (assoc (riece-format-identity identity) riece-log-directory-map)))
    (if map
	(expand-file-name (cdr map) riece-log-directory)
      (expand-file-name (riece-log-encode-file-name prefix)
			(expand-file-name
			 (concat "." (riece-log-encode-file-name server))
			 riece-log-directory)))))

(defun riece-log-encode-file-name (file-name)
  (if riece-log-file-name-coding-system
      (setq file-name
	    (encode-coding-string file-name
				  riece-log-file-name-coding-system)))
  (let ((index 0)
	c)
    (while (string-match "[^-0-9A-Za-z_\x80-\xFF]" file-name index)
      (setq c (aref file-name (match-beginning 0)))
      (if (eq c ?=)
	  (setq file-name (replace-match "==" nil t file-name)
		index (1+ (match-end 0)))
	(setq file-name (replace-match (format "=%02X" c) nil t file-name)
	      index (+ 2 (match-end 0)))))
    file-name))

(defun riece-log-decode-file-name (file-name)
  (let ((index 0))
    (while (string-match "==\\|=\\([0-7][0-9A-F]\\)" file-name index)
      (setq file-name (replace-match
		       (if (eq (aref file-name (1- (match-end 0))) ?=)
			   "="
			 (char-to-string
			  (car (read-from-string
				(concat "?\\x" (match-string 1 file-name))))))
		       nil t file-name)
	    index (1+ (match-beginning 0))))
    file-name)
  (if riece-log-file-name-coding-system
      (setq file-name
	    (decode-coding-string file-name
				  riece-log-file-name-coding-system)))
  file-name)

(defun riece-log-insert (identity lines)
  "Insert logs for IDENTITY at most LINES.
If LINES is t, insert today's logs entirely."
  (if (eq lines t)
      (let* (file-name-coding-system
	     default-file-name-coding-system
	     (file (riece-log-get-file identity)))
	(if (file-exists-p file)
	    (insert-file-contents file)))
    (let* (file-name-coding-system
	   default-file-name-coding-system
	   (files (riece-log-get-files identity))
	   (lines (- lines))
	   name date point)
      (while (and (< lines 0) files)
	(if (and (file-exists-p (car files))
		 (string-match (concat (riece-make-interval-regexp "[0-9]" 8)
				       "\\.txt$")
			       (setq name (file-name-nondirectory
					   (car files)))))
	    (save-restriction
	      (narrow-to-region (point) (point))
	      (insert-file-contents (car files))
	      (goto-char (point-max))
	      (setq lines (forward-line lines))
	      (delete-region (point-min) (point))
	      (unless (equal name (format-time-string "%Y%m%d.txt"))
		(setq date (concat " (" (substring name 0 4) "/"
				   (substring name 4 6) "/"
				   (substring name 6 8) ")"))
		(while (not (eobp))
		  (end-of-line)
		  (setq point (point))
		  (insert date)
		  (put-text-property point (point)
				     'riece-overlay-face 'riece-log-date-face)
		  (forward-line))
		(goto-char (point-min)))))
	(setq files (cdr files))))))

(defun riece-log-flashback (identity)
  (when riece-log-flashback
    (riece-insert-info (current-buffer)
		       (if (eq riece-log-flashback t)
			   "Recent messages of the day:\n"
			 (format "Recent messages up to %d lines:\n"
				 riece-log-flashback)))
    (let (buffer-read-only
	  (point (goto-char (point-max))))
      (insert (with-temp-buffer
		(riece-log-insert identity riece-log-flashback)
		(buffer-string)))
      (goto-char point)
      (while (re-search-forward
	      (concat "^" riece-time-prefix-regexp
		       "\\(<[^>]+>\\|>[^<]+<\\|([^)]+)\\|{[^}]+}\\|=[^=]+=\\)")
	      nil t)
	(put-text-property (1+ (match-beginning 1)) (1- (match-end 1))
			   'riece-identity
			   (riece-make-identity
			    (buffer-substring (1+ (match-beginning 1))
					      (1- (match-end 1)))
			    (riece-identity-server identity))))
      (run-hook-with-args 'riece-after-insert-functions
			  point (goto-char (point-max)))
      (set-window-point (get-buffer-window (current-buffer))
			(point)))))

(defun riece-log-open-directory (&optional channel)
  (interactive)
  (let ((directory (riece-log-get-directory
		    (or channel riece-current-channel))))
    (if (file-directory-p directory)
	(funcall riece-log-open-directory-function directory)
      (error "No log directory"))))

(defun riece-log-requires ()
  (if (memq 'riece-button riece-addons)
      '(riece-button)))

(defun riece-log-insinuate ()
  ;; FIXME: Use `riece-after-insert-functions' for trapping change,
  ;; notice, wallops and so on. But must add argument.
  (add-hook 'riece-after-display-message-functions
	    'riece-log-display-message-function)
  (add-hook 'riece-channel-buffer-create-functions
	    'riece-log-flashback))

(defvar riece-command-mode-map)
(defun riece-log-enable ()
  (define-key riece-command-mode-map "\C-cd" 'riece-log-open-directory)
  (make-directory riece-log-directory t)
  (condition-case nil
      (progn
	(make-directory riece-log-lock-directory)
	(add-hook 'riece-exit-hook
		  (lambda ()
		    (condition-case nil
			(delete-directory riece-log-lock-directory)
		      (error))))
	(setq riece-log-enabled t))
    (error
     (if riece-debug
	 (message "Can't create lock directory for riece-log")))))

(defun riece-log-disable ()
  (define-key riece-command-mode-map "\C-cd" nil)
  (setq riece-log-enabled nil))

(provide 'riece-log)

;;; riece-log.el ends here
