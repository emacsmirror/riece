;;; riece-log.el --- saving irc logs add-on
;; Copyright (C) 2003 OHASHI Akira

;; Author: OHASHI Akira <bg66@koka-in.org>
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

(defcustom riece-log-directory-map nil
  "*The map of channel name and directory name."
  :type '(repeat (cons (string :tag "Channel name")
		       (string :tag "Directory name")))
  :group 'riece-log)

(defcustom riece-log-flashback 10
  "*If non-nil, irc messages flash back from log files.
If integer, flash back only this line numbers. t means all lines."
  :type '(choice (integer :tag "line numbers")
		 (boolean :tag "flash back or not"))
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

(defvar riece-log-enabled nil)

(defconst riece-log-description
  "Saving IRC logs")

(defun riece-log-display-message-function (message)
  (if riece-log-enabled
      (let ((file (riece-log-get-file (riece-message-target message)))
	    (coding-system-for-write riece-log-coding-system))
	(unless (file-directory-p (file-name-directory file))
	  (make-directory (file-name-directory file) t))
	(write-region (concat (format-time-string "%H:%M") " "
			      (riece-format-message message))
		      nil file t 0))))

(defun riece-log-get-file (identity)
  (expand-file-name
   (concat (format-time-string "%Y%m%d") ".log")
   (riece-log-get-directory identity)))

(defun riece-log-get-files (identity)
  (let ((files (directory-files (riece-log-get-directory identity) t
				(concat "^"
					(riece-make-interval-regexp "[0-9]" 8)
					"\\.log$")
				t)))
    (nreverse (sort files #'string-lessp))))

(defun riece-log-get-directory (identity)
  (let ((channel (riece-identity-canonicalize-prefix
		  (riece-identity-prefix identity)))
	(server (riece-identity-server identity))
	(map (assoc (riece-format-identity identity) riece-log-directory-map))
	name)
    (cond (map (setq name (cdr map)))
	  ((string-match riece-strict-channel-regexp channel)
	   (let ((suffix (match-string 2 channel)))
	     (setq name (substring channel (match-end 1) (match-beginning 2)))
	     (when (and (stringp suffix)
			(string-match "^:\\*\\.\\(.*\\)" suffix))
	       (setq name (concat name "-" (match-string 1 suffix))))))
	  (t (setq name "priv")))
    (if server
	(expand-file-name name (expand-file-name server riece-log-directory))
      (expand-file-name name riece-log-directory))))

(defun riece-log-get-directory-1 (identity)
  (let ((prefix (riece-identity-canonicalize-prefix
		 (riece-identity-prefix identity)))
	(server (riece-identity-server identity))
	(map (assoc (riece-format-identity identity) riece-log-directory-map))
	name)
    (if map
	(setq name (cdr map))
      (expand-file-name (riece-log-encode-file-name prefix)
			(expand-file-name
			 (riece-log-encode-file-name (concat "." server))
			 riece-log-directory)))))

(defun riece-log-encode-file-name (file-name)
  (if riece-log-file-name-coding-system
      (setq file-name
	    (encode-coding-string file-name
				  riece-log-file-name-coding-system)))
  (let ((index 0)
	c)
    (while (string-match "[^-0-9A-Za-z=_\x80-\xFF]" file-name index)
      (setq c (aref file-name (match-beginning 0))
	    file-name (replace-match
		       (if (eq c ?=)
			   "=="
			 (format "=%02X" c))
		       nil t file-name)
	    index (+ 3 index)))
    file-name))

(defun riece-log-decode-file-name (file-name)
  (let ((index 0))
    (while (string-match "==\\|=\\([0-7][0-9A-F]\\)" file-name index)
      (setq file-name (replace-match
		       (if (eq (aref file-name (match-end 0)) ?=)
			   "="
			 (char-to-string
			  (car (read-from-string
				(concat "?\\x" (match-string 1 file-name))))))
		       nil t file-name)
	    index (1+ index)))
    file-name)
  (if riece-log-file-name-coding-system
      (setq file-name
	    (decode-coding-string file-name
				  riece-log-file-name-coding-system)))
  file-name)

(defun riece-log-flashback-1 (identity)
  (if (eq riece-log-flashback t)
      (let ((file (riece-log-get-file identity)))
	(if (file-exists-p file)
	    (insert-file-contents file)))
    (let ((files (riece-log-get-files identity))
	  (lines (- riece-log-flashback))
	  date point)
      (while (and (< lines 0) files)
	(if (and (file-exists-p (car files))
		 (string-match (concat "\\("
				       (riece-make-interval-regexp "[0-9]" 4)
				       "\\)\\("
				       (riece-make-interval-regexp "[0-9]" 2)
				       "\\)\\("
				       (riece-make-interval-regexp "[0-9]" 2)
				       "\\).log$")
			       (car files)))
	    (save-restriction
	      (narrow-to-region (point-min) (point-min))
	      (setq date (concat " (" (match-string 1 (car files)) "/"
				 (match-string 2 (car files)) "/"
				 (match-string 3 (car files)) ")"))
	      (insert-file-contents (car files))
	      (goto-char (point-max))
	      (setq lines (forward-line lines)
		    point (point))
	      (while (not (eobp))
		(end-of-line)
		(insert date)
		(forward-line))
	      (goto-char point)))
	(setq files (cdr files)))
      (if (zerop lines)
	  (delete-region (point-min) (point))))))

(defun riece-log-flashback (identity)
  (when riece-log-flashback
    (let (buffer-read-only
	  (point (goto-char (point-max))))
      (insert (with-temp-buffer
		(riece-log-flashback-1 identity)
		(buffer-string)))
      (goto-char point)
      (while (re-search-forward
	      "^[0-9][0-9]:[0-9][0-9] [<>]\\([^<>]+\\)[<>] " nil t)
	(put-text-property (match-beginning 1) (match-end 1)
			   'riece-identity
			   (riece-make-identity
			    (riece-match-string-no-properties 1)
			    (riece-identity-server identity))))
      (when (and (memq 'riece-button riece-addons)
		 riece-button-enabled)
	(riece-button-update-buffer))
      (goto-char (point-max))
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
  (setq riece-log-enabled t))

(defun riece-log-disable ()
  (define-key riece-command-mode-map "\C-cd" nil)
  (setq riece-log-enabled nil))

(provide 'riece-log)

;;; riece-log.el ends here
