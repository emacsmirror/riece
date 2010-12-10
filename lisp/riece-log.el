;;; riece-log.el --- Save IRC logs
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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; NOTE: This is an add-on module for Riece.

;;; Code:

(require 'riece-message)
(require 'riece-button)
(require 'riece-mcat)

(defgroup riece-log nil
  "Save IRC logs."
  :prefix "riece-"
  :group 'riece)

(defcustom riece-log-directory
  (expand-file-name "log" riece-directory)
  "*Where to look for log files."
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

(defvar riece-log-lock-file nil
  "Lock file for riece-log.
It is created if there is at least one instance of Emacs running riece-log.")

(defconst riece-log-file-name-regexp
  (concat (riece-make-interval-regexp "[0-9]" 8) "\\.txt\\(\\.\\(.*\\)\\)?$"))

(defconst riece-log-description
  "Save IRC logs.")

(defun riece-log-display-message-function (message)
  (if (get 'riece-log 'riece-addon-enabled)
      (let* ((coding-system-for-write
	      (if (featurep 'mule)
		  (or riece-log-coding-system
		      (car (get-language-info current-language-environment
					      'coding-system)))))
	     (file (riece-log-make-file-name (riece-message-target message)
					     coding-system-for-write))
	     (file-name-coding-system 'no-conversion))
	(unless (file-directory-p (file-name-directory file))
	  (make-directory (file-name-directory file) t))
	(write-region (concat (format-time-string "%H:%M") " "
			      (riece-format-message message))
		      nil file t 0
		      riece-log-lock-file))))

(defun riece-log-make-file-name (identity coding-system)
  (expand-file-name (if (and (featurep 'mule) coding-system)
			(format "%s.txt.%s"
				(format-time-string "%Y%m%d")
				coding-system)
		      (format "%s.txt"
				(format-time-string "%Y%m%d")))
		    (riece-log-directory identity)))

(defun riece-log-list-files (identity time)
  (let ((directory (riece-log-directory identity))
	(time-prefix (format-time-string "%Y%m%d" (or time '(0 0))))
	files)
    (when (file-directory-p directory)
      (setq files (nreverse (sort (directory-files
				   directory t
				   (concat "^" riece-log-file-name-regexp)
				   t)
				  #'string-lessp)))
      (while (and files
		  (string-lessp (file-name-nondirectory (car files))
				time-prefix))
	(setq files (cdr files)))
      files)))

(defun riece-log-directory (identity)
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
  (let* ((file-name-coding-system 'no-conversion)
	 (files (riece-log-list-files identity
				      (if (eq lines t) (current-time))))
	 name coding-system date point)
    (while (and (or (eq lines t) (> lines 0)) files)
      (save-restriction
	(narrow-to-region (point) (point))
	(if (and (string-match
		  (concat "^" riece-log-file-name-regexp)
		  (setq name (file-name-nondirectory (car files))))
		 (match-beginning 2))
	    (progn
	      (setq coding-system
		    (intern (substring name (match-beginning 2))))
	      (if (featurep 'xemacs)
		  (setq coding-system (find-coding-system coding-system))
		(unless (coding-system-p coding-system)
		  (setq coding-system nil)))
	      (if coding-system
		  (let ((coding-system-for-read coding-system))
		    (insert-file-contents (car files)))
		;;don't insert file contents if they use non
		;;supported coding-system.
		))
	  ;;if the filename has no coding-system suffix, decode with
	  ;;riece-log-coding-system.
	  (let ((coding-system-for-read riece-log-coding-system))
	    (insert-file-contents (car files))))
	;;lines in the file contents are in reversed order.
	(unless (eq lines t)
	  (goto-char (point-max))
	  (setq lines (- (forward-line (- lines))))
	  (delete-region (point-min) (point)))
	;;add (YYYY/MM/dd) suffix on each line left in the current buffer.
	(unless (equal (substring name 0 8) (format-time-string "%Y%m%d"))
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
	  (goto-char (point-min))))
      (setq files (cdr files)))))

(defun riece-log-flashback (identity)
  (when riece-log-flashback
    (riece-insert-info (current-buffer)
		       (if (eq riece-log-flashback t)
			   (riece-mcat "Recent messages of the day:\n")
			 (format (riece-mcat
				  "Recent messages up to %d lines:\n")
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

(defun riece-log-dired (&optional channel)
  (interactive)
  (let ((directory (riece-log-directory (or channel riece-current-channel))))
    (if (file-directory-p directory)
	(dired directory)
      (error "No log directory"))))

(defun riece-log-requires ()
  (if (memq 'riece-button riece-addons)
      '(riece-button)))

(defun riece-log-insinuate ()
  (make-directory riece-log-directory t)
  (setq riece-log-lock-file
	(expand-file-name (format "!%s-%d-%d"
				  (riece-log-encode-file-name (system-name))
				  (user-uid)
				  (emacs-pid))
			  riece-log-directory))
  ;; FIXME: Use `riece-after-insert-functions' for trapping change,
  ;; notice, wallops and so on. But must add argument.
  (add-hook 'riece-after-display-message-functions
	    'riece-log-display-message-function)
  (add-hook 'riece-channel-buffer-create-functions
	    'riece-log-flashback))

(defun riece-log-uninstall ()
  (setq riece-log-lock-file nil)
  (remove-hook 'riece-after-display-message-functions
	       'riece-log-display-message-function)
  (remove-hook 'riece-channel-buffer-create-functions
	       'riece-log-flashback))

(defvar riece-command-mode-map)
(defun riece-log-enable ()
  (define-key riece-command-mode-map "\C-cd" 'riece-log-dired))

(defun riece-log-disable ()
  (define-key riece-command-mode-map "\C-cd" nil))

(provide 'riece-log)

;;; riece-log.el ends here
