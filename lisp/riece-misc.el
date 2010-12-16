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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'riece-options)
(require 'riece-coding)
(require 'riece-identity)
(require 'riece-version)
(require 'riece-channel)
(require 'riece-server)
(require 'riece-user)
(require 'riece-mode)
(require 'riece-cache)

(defun riece-get-buffer-create (name &optional init-major-mode)
  (let ((buffer (get-buffer name)))
    (unless (and buffer
		 (or (null init-major-mode)
		     (eq (with-current-buffer buffer
			   major-mode)
			 init-major-mode)))
      (setq buffer (generate-new-buffer name)))
    (unless (memq buffer riece-buffer-list)
      (setq riece-buffer-list (cons buffer riece-buffer-list)))
    buffer))

(defun riece-scan-property-region (property start end function)
  (catch 'done
    (while t
      ;; Search for the beginning of the property region.
      (unless (get-text-property start property)
	(setq start (next-single-property-change start property nil end)))
      (if (= start end)
	  (throw 'done nil))
      ;; Search for the end of the property region.
      (let ((region-end (next-single-property-change start property nil end)))
	(if (= region-end end)
	    (throw 'done nil))
	(funcall function start region-end)
	(setq start region-end)))))

(defun riece-insert (buffers string)
  (unless (listp buffers)
    (setq buffers (list buffers)))
  (while buffers
    (run-hooks 'riece-before-insert-functions)
    (with-current-buffer (car buffers)
      (let ((inhibit-read-only t)
	    buffer-read-only
	    start
	    window
	    point)
	;; Save the current for the case when (car buffers) is the
	;; currently selected buffer.
	(save-excursion
	  (setq start (goto-char (point-max)))
	  (insert (format-time-string "%H:%M") " " string)
	  (setq point (point)))
	(if (and (not (riece-frozen (current-buffer)))
		 (setq window (get-buffer-window (current-buffer)))
		 (not (pos-visible-in-window-p point window)))
	    (save-excursion		;save-selected-window changes
					;current buffer
	      (save-selected-window
		(select-window window)
		(goto-char point)	;select-window changes current point
		(recenter riece-window-center-line))))
	(run-hook-with-args 'riece-after-insert-functions start point)))
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

(defun riece-frozen (buffer)
  (with-current-buffer buffer
    riece-freeze))

(defun riece-own-frozen (buffer)
  (with-current-buffer buffer
    (eq riece-freeze 'own)))

(defun riece-channel-p (string)
  "Return t if STRING is a channel.
\(i.e. it matches `riece-channel-regexp')"
  (string-match (concat "^" riece-channel-regexp) string))

(defun riece-user-p (string)
  "Return t if STRING is a user.
\(i.e. it matches `riece-user-regexp')"
  (string-match (concat "^" riece-user-regexp) string))

(defun riece-current-nickname ()
  "Return the current nickname."
  (riece-with-server-buffer (riece-current-server-name)
    (if riece-real-nickname
	(riece-make-identity riece-real-nickname riece-server-name))))

(defun riece-split-parameters (string)
  (if (eq ?: (aref string 0))
      (list (substring string 1))
    (let (parameters)
      (catch 'done
	(while (string-match "^\\([^ ]+\\) +" string)
	  (setq parameters (nconc parameters (list (match-string 1 string)))
		string (substring string (match-end 0)))
	  (when (and (not (equal "" string)) (eq ?: (aref string 0)))
	    (setq string (substring string 1)
		  parameters (nconc parameters (list string)))
	    (throw 'done nil)))
	(or (equal "" string)
	    (setq parameters (nconc parameters (list string)))))
      parameters)))

(defun riece-concat-channel-topic (target string)
  (riece-with-server-buffer (riece-identity-server target)
    (let ((topic (riece-channel-get-topic (riece-identity-prefix target))))
      (if (or (null topic)
	      (equal topic ""))
	  string
	(concat string ": " topic)))))

(defun riece-concat-channel-modes (target string)
  (riece-with-server-buffer (riece-identity-server target)
    (let ((modes (riece-channel-get-modes (riece-identity-prefix target))))
      (if modes
	  (concat string " ["
		  (mapconcat
		   (lambda (mode)
		     (if (riece-mode-parameter mode)
			 (format "%c(%s)"
				 (riece-mode-flag mode)
				 (riece-mode-parameter mode))
		       (char-to-string (riece-mode-flag mode))))
		   modes "")
		  "]")
	string))))

(defun riece-concat-message (string message)
  (if (or (null message)
	  (equal message ""))
      string
    (concat string " (" message ")")))

(defun riece-concat-server-name (string)
  (if (equal riece-server-name "")
      string
    (let ((server-name (concat " (from " riece-server-name ")")))
      (put-text-property 0 (length server-name)
			 'riece-server-name riece-server-name
			 server-name)
      (concat string server-name))))

(defun riece-concat-user-status (status string)
  (if status
      (concat string " [" (mapconcat #'identity status ", ") "]")
    string))

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

(defun riece-get-users-on-server (server-name)
  (riece-with-server-buffer server-name
    (let (identities)
      (mapatoms
       (lambda (user)
	 (setq identities
	       (cons (riece-make-identity (symbol-name user) server-name)
		     identities)))
       (riece-cache-hash-obarray riece-user-cache))
      identities)))

(defun riece-get-channels-on-server (server-name)
  (riece-with-server-buffer server-name
    (let (identities)
      (mapatoms
       (lambda (channel)
	 (setq identities
	       (cons (riece-make-identity (symbol-name channel) server-name)
		     identities)))
       (riece-cache-hash-obarray riece-channel-cache))
      identities)))

(defun riece-get-identities-on-server (server-name)
  (nconc (riece-get-channels-on-server server-name)
	 (riece-get-users-on-server server-name)))

(defun riece-check-channel-commands-are-usable (&optional channel)
   (unless riece-current-channel
     (error (substitute-command-keys
	     "Type \\[riece-command-join] to join a channel")))
   (if (and channel
	    (not (riece-channel-p (riece-identity-prefix
				   riece-current-channel))))
       (error "Not on a channel")))

(provide 'riece-misc)

;;; riece-misc.el ends here
