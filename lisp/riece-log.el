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

(eval-when-compile (require 'riece-message))

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

(defun riece-log-display-message-function (message)
  (let ((open-bracket
	 (funcall riece-message-make-open-bracket-function message))
	(close-bracket
	 (funcall riece-message-make-close-bracket-function message))
	(name
	 (funcall riece-message-make-name-function message))
	(file (riece-log-get-file (riece-message-target message))))
    (unless (file-directory-p (file-name-directory file))
      (make-directory (file-name-directory file) t))
    (with-temp-buffer
      (insert (concat (format-time-string "%H:%M") " "
		      open-bracket name close-bracket
		      " " (riece-message-text message) "\n"))
      (write-region (point-min) (point-max) file t 0))))

(defun riece-log-get-file (identity)
  (expand-file-name
   (concat (format-time-string "%Y%m%d") ".log")
   (riece-log-get-directory identity)))

(defun riece-log-get-directory (identity)
  (let ((channel (riece-identity-prefix identity))
	(server (riece-identity-server identity)))
    (let ((map (assoc channel riece-log-directory-map)))
      (if map
	  (expand-file-name (cdr map) riece-log-directory)
	(if (string-match (concat riece-channel-regexp
				  "\\([^:]+\\)\\(:\\*\\.\\(.*\\)\\)?") channel)
	    (let ((name (match-string 1 channel))
		  (suffix (match-string 3 channel)))
	      (let ((name (if suffix (concat name "-" suffix) name)))
		(if server
		    (expand-file-name
		     name
		     (expand-file-name server riece-log-directory))
		  (expand-file-name name riece-log-directory))))
	  riece-log-directory)))))

(defun riece-log-flashback (identity)
  (when riece-log-flashback
    (let ((file (riece-log-get-file identity)))
      (when (file-exists-p file)
	(let (string)
	  (with-temp-buffer
	    (insert-file-contents file)
	    (if (not (integerp riece-log-flashback))
		(goto-char (point-min))
	      (goto-char (point-max))
	      (forward-line (- riece-log-flashback)))
	    (setq string (buffer-substring (point) (point-max))))
	  (let (buffer-read-only)
	    (goto-char (point-max))
	    (insert string)
	    (goto-char (point-max))
	    (set-window-point (get-buffer-window (current-buffer))
			      (point))))))))

(defun riece-log-open-directory (&optional channel)
  (interactive)
  (if channel
      (find-file (riece-log-get-directory channel))
    (find-file riece-log-directory)))

(defun riece-log-insinuate ()
  ;; FIXME: Use `riece-after-insert-functions' for trapping change,
  ;; notice, wallops and so on. But must add argument.
  (add-hook 'riece-after-display-message-functions
	    'riece-log-display-message-function)
  (add-hook 'riece-channel-buffer-create-functions
	    'riece-log-flashback))

(provide 'riece-log)

;;; riece-log.el ends here
