;;; riece-misc.el --- miscellaneous functions (not inlined)
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

(eval-when-compile (require 'riece-inlines))

(require 'riece-options)
(require 'riece-coding)
(require 'riece-identity)
(require 'riece-version)
(require 'riece-channel)
(require 'riece-user)

(defun riece-get-buffer-create (name)
  (let ((buffer (get-buffer-create name)))
    (unless (memq buffer riece-buffer-list)
      (setq riece-buffer-list (cons buffer riece-buffer-list)))
    buffer))

(defun riece-insert (buffers string)
  (unless (listp buffers)
    (setq buffers (list buffers)))
  (while buffers
    (run-hooks 'riece-before-insert-functions)
    (save-excursion
      (set-buffer (riece-get-buffer-create (car buffers)))
      (let ((inhibit-read-only t)
	    buffer-read-only
	    (start (goto-char (point-max))))
	(insert (format-time-string "%H:%M") " " string)
	(if (and (not (riece-frozen (current-buffer)))
		 (get-buffer-window (current-buffer)))
	    (set-window-point (get-buffer-window (current-buffer))
			      (point)))
	(run-hook-with-args 'riece-after-insert-functions start (point))))
    (setq buffers (cdr buffers))))

(defun riece-insert-change (buffer message)
  (riece-insert buffer (concat riece-change-prefix message)))

(defun riece-insert-notice (buffer message)
  (riece-insert buffer (concat riece-notice-prefix message)))

(defun riece-insert-wallops (buffer message)
  (riece-insert buffer (concat riece-wallops-prefix message)))

(defun riece-insert-error (buffer message)
  (riece-insert buffer (concat riece-error-prefix message)))

(defun riece-insert-info (buffer message)
  (riece-insert buffer (concat riece-info-prefix message)))

(defun riece-freeze (buffer &optional arg)
  (with-current-buffer buffer
    (setq riece-freeze (if arg (< 0 arg) (not riece-freeze))
	  riece-freeze-indicator (if riece-freeze "F" "-"))
    (force-mode-line-update)))

(defun riece-frozen (buffer)
  (with-current-buffer buffer riece-freeze))

(defun riece-own-freeze (buffer &optional arg)
  (with-current-buffer buffer
    (setq riece-own-freeze (if arg (< 0 arg) (not riece-own-freeze))
	  riece-own-freeze-indicator (if riece-own-freeze "M" "-"))
    (force-mode-line-update)))

(defun riece-process-send-string (process string)
  (with-current-buffer (process-buffer process)
    (process-send-string process (riece-encode-coding-string string))))

(defun riece-send-string (string)
  (let ((process (riece-find-server-process)))
    (unless process
      (error "%s" (substitute-command-keys
		   "Type \\[riece-command-open-server] to open server.")))
    (riece-process-send-string process string)))

(defun riece-split-parameters (string)
  (if (eq ?: (aref string 0))
      (list (substring string 1))
    (let (parameters)
      (catch 'done
	(while (string-match "^\\([^ ]+\\) +" string)
	  (setq parameters (nconc parameters (list (match-string 1 string)))
		string (substring string (match-end 0)))
	  (and (not (equal "" string)) (eq ?: (aref string 0))
	       (setq string (substring string 1))
	       (throw 'done nil))))
      (or (equal "" string)
	  (setq parameters (nconc parameters (list string))))
      parameters)))

(defun riece-concat-modes (target string)
  (let ((modes
	 (if (riece-channel-p target)
	     (riece-channel-get-modes target)
	   (riece-user-get-modes target))))
    (if modes
	(concat string " [" (apply #'string modes) "]")
      string)))

(defsubst riece-concat-current-channel-modes (string)
  (if riece-current-channel
      (riece-concat-modes riece-current-channel string)
    string))

(defun riece-concat-message (string message)
  (if (or (null message)
	  (equal message ""))
      string
    (concat string " (" message ")")))

(defun riece-concat-server-name (string)
  (riece-with-server-buffer
   (if riece-server-name
       (concat string " (from " riece-server-name ")")
     string)))

(defun riece-prefix-user-at-host (prefix)
  (if (string-match "!" prefix)
      (substring prefix (match-end 0))
    prefix))

(defun riece-prefix-nickname (prefix)
  (if (string-match "!" prefix)
      (substring prefix 0 (match-beginning 0))
    prefix))

(defun riece-parse-user-at-host (user-at-host)
  (if (memq (aref user-at-host 0) '(?^ ?= ?~ ?- ?+))
      (progn
	(if (memq (aref user-at-host 0) '(?^ ?=))
	    (setq riece-user-at-host-type 'fake)
	  (if (memq (aref user-at-host 0) '(?~ ?-))
	      (setq riece-user-at-host-type 'not-verified)
	    (if (eq (aref user-at-host 0) ?+)
		(setq riece-user-at-host-type 'ok))))
	(substring user-at-host 1))
    (setq riece-user-at-host-type 'ok)
    user-at-host))

(defun riece-strip-user-at-host (user-at-host)
  (if (memq (aref user-at-host 0) '(?^ ?= ?~ ?- ?+))
      (substring user-at-host 1)
    user-at-host))

(defun riece-get-users-on-server ()
  (riece-with-server-buffer
   (let (users)
     (mapatoms
      (lambda (atom)
	(unless (riece-channel-p (symbol-name atom))
	  (setq users (cons (symbol-name atom) users))))
      riece-obarray)
     (if (member riece-real-nickname users)
	 users
       (cons riece-real-nickname users)))))

(provide 'riece-misc)

;;; riece-misc.el ends here
