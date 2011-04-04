;;; riece-ndcc.el --- DCC file sending protocol support (written in elisp) -*- lexical-binding: t -*-
;; Copyright (C) 1998-2003 Daiki Ueno

;; Author: Daiki Ueno <ueno@unixuser.org>
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

(require 'riece-globals)
(require 'riece-options)

(defgroup riece-ndcc nil
  "DCC written in elisp."
  :prefix "riece-"
  :group 'riece)

(defcustom riece-ndcc-server-address "127.0.0.1"
  "Local address of the DCC server.
Only used for sending files."
  :type 'vector
  :group 'riece-ndcc)

(defvar riece-ndcc-requests nil)

(defvar riece-ndcc-request-user nil)
(defvar riece-ndcc-request-size nil)

(defconst riece-ndcc-description
  "DCC file sending protocol support (written in elisp.)")

(defun riece-ndcc-encode-address (address)
  (unless (string-match
	   "^\\([0-9]+\\)\\.\\([0-9]+\\)\\.\\([0-9]+\\)\\.\\([0-9]+\\)$"
	   address)
    (error "% is not an IP address" address))
  (let ((string (number-to-string
		 (+ (* (float (string-to-number (match-string 1 address)))
		       16777216)
		    (* (float (string-to-number (match-string 2 address)))
		       65536)
		    (* (float (string-to-number (match-string 3 address)))
		       256)
		    (float (string-to-number (match-string 4 address)))))))
    (if (string-match "\\." string)
	(substring string 0 (match-beginning 0))
      string)))

(defun riece-ndcc-decode-address (address)
  (let ((float-address (string-to-number (concat address ".0"))))
    (format "%d.%d.%d.%d"
	    (floor (mod (/ float-address 16777216) 256))
	    (floor (mod (/ float-address 65536) 256))
	    (floor (mod (/ float-address 256) 256))
	    (floor (mod float-address 256)))))

(defun riece-ndcc-server-sentinel (process status)
  (when (string-match "^open from " status)
    (let ((parent-name
	   (if (string-match " <[^>]+>$" (process-name process))
	       (substring (process-name process) 0 (match-beginning 0)))))
      (save-excursion
	(set-buffer (process-buffer (get-process parent-name)))
	(goto-char (point-min))
	(while (not (eobp))
	  (process-send-region process
			       (point)
			       (goto-char (min (+ 1024 (point)) (point-max))))
	  (message "Sending %s...(%d/%d)"
		   (buffer-file-name) (1- (point)) (buffer-size)))
	(message "Sending %s...done"
		 (buffer-file-name)))
      (kill-buffer (process-buffer (get-process parent-name))))
    (kill-buffer (process-buffer process))))

(defun riece-command-dcc-send (user file)
  (interactive
   (let ((completion-ignore-case t))
     (unless riece-ndcc-server-address
       (error "Set riece-ndcc-server-address to your host"))
     (list (riece-completing-read-identity
	    "User: "
	    (riece-get-users-on-server (riece-current-server-name)))
	   (expand-file-name (read-file-name "File: ")))))
  (let* (selective-display
	 (coding-system-for-read 'binary)
	 format-alist
	 jka-compr-compression-info-list
	 (buffer (find-file-noselect file))
	 process)
    (with-current-buffer buffer		;To throw an error when the
      (setq buffer-read-only t))	;process has input.
    (setq process (make-network-process :name "DCC" :buffer buffer
					:host riece-ndcc-server-address
					:server t :service t
					:coding 'binary
					:sentinel 'riece-ndcc-server-sentinel))
    (riece-send-string
     (format "PRIVMSG %s :\1DCC SEND %s %s %d %d\1\r\n"
	     (riece-identity-prefix user)
	     (file-name-nondirectory file)
	     (riece-ndcc-encode-address riece-ndcc-server-address)
	     (nth 1 (process-contact process))
	     (nth 7 (file-attributes file))))))

(defun riece-ndcc-filter (process input)
  (save-excursion
    (set-buffer (process-buffer process))
    (goto-char (point-max))
    (insert input)
    (message "Receiving %s from %s...(%d/%d)"
	     (file-name-nondirectory buffer-file-name)
	     riece-ndcc-request-user
	     (1- (point))
	     riece-ndcc-request-size)))

(defun riece-ndcc-sentinel (process status)
  (save-excursion
    (set-buffer (process-buffer process))
    (unless (= (buffer-size) riece-ndcc-request-size)
      (error "Premature end of file"))
    (message "Receiving %s from %s...done"
	     (file-name-nondirectory buffer-file-name)
	     riece-ndcc-request-user)
    (let ((coding-system-for-write 'binary))
      (save-buffer))))

(defun riece-command-dcc-receive (request file)
  (interactive
   (progn
     (unless riece-ndcc-requests
       (error "No request"))
     (list
      (if (= (length riece-ndcc-requests) 1)
	  (car riece-ndcc-requests)
	(with-output-to-temp-buffer "*Help*"
	  (let ((requests riece-ndcc-requests)
		(index 1))
	    (while requests
	      (princ (format "%2d: %s %s (%d bytes)\n"
			     index
			     (car (car requests))
			     (nth 1 (car requests))
			     (nth 4 (car requests))))
	      (setq index (1+ index)
		    requests (cdr requests)))))
	(let ((number (read-string "Request#: ")))
	  (unless (string-match "^[0-9]+$" number)
	    (error "Not a number"))
	  (if (or (> (setq number (string-to-number number))
		     (length riece-ndcc-requests))
		  (< number 1))
	      (error "Invalid number"))
	  (nth (1- number) riece-ndcc-requests)))
      (expand-file-name (read-file-name "Save as: ")))))
  (let* (selective-display
	 (coding-system-for-read 'binary)
	 (coding-system-for-write 'binary)
	 (process (open-network-stream
		   "DCC" " *DCC*"
		   (riece-ndcc-decode-address (nth 2 request))
		   (nth 3 request))))
    (setq riece-ndcc-requests (delq request riece-ndcc-requests))
    (with-current-buffer (process-buffer process)
      (set-buffer-multibyte nil)
      (buffer-disable-undo)
      (setq buffer-file-name file)
      (make-local-variable 'riece-ndcc-request-user)
      (setq riece-ndcc-request-user (car request))
      (make-local-variable 'riece-ndcc-request-size)
      (setq riece-ndcc-request-size (nth 4 request)))
    (set-process-filter process #'riece-ndcc-filter)
    (set-process-sentinel process #'riece-ndcc-sentinel)))

(defun riece-handle-dcc-request (prefix target message)
  (let ((case-fold-search t))
    (when (and (get 'riece-ndcc 'riece-addon-enabled)
	       (string-match
		"SEND \\([^ ]+\\) \\([^ ]+\\) \\([^ ]+\\) \\([^ ]+\\)"
		message))
      (let ((file (match-string 1 message))
	    (address (match-string 2 message))
	    (port (string-to-number (match-string 3 message)))
	    (size (string-to-number (match-string 4 message)))
	    (buffer (if (riece-channel-p target)
			(riece-channel-buffer (riece-make-identity
					       target riece-server-name))))
	    (user (riece-prefix-nickname prefix)))
	(setq riece-ndcc-requests
	      (cons (list user file address port size)
		    riece-ndcc-requests))
	(riece-insert-change buffer (format "DCC SEND from %s\n" user))
	(riece-insert-change
	 (if (and riece-channel-buffer-mode
		  (not (eq buffer riece-channel-buffer)))
	     (list riece-dialogue-buffer riece-others-buffer)
	   riece-dialogue-buffer)
	 (concat
	  (riece-concat-server-name
	   (format "DCC SEND from %s (%s) to %s"
		   user
		   (riece-strip-user-at-host
		    (riece-prefix-user-at-host prefix))
		   target))
	  "\n")))
      t)))

(defun riece-ndcc-requires ()
  '(riece-ctcp))

(defvar riece-dialogue-mode-map)
(defun riece-ndcc-insinuate ()
  (unless (fboundp 'make-network-process)
    (error "This Emacs does not have make-network-process"))
  (add-hook 'riece-ctcp-dcc-request-hook 'riece-handle-dcc-request))

(defun riece-ndcc-uninstall ()
  (remove-hook 'riece-ctcp-dcc-request-hook 'riece-handle-dcc-request))

(defun riece-ndcc-enable ()
  (define-key riece-dialogue-mode-map "\C-ds" 'riece-command-dcc-send)
  (define-key riece-dialogue-mode-map "\C-dr" 'riece-command-dcc-receive))

(defun riece-ndcc-disable ()
  (define-key riece-dialogue-mode-map "\C-ds" nil)
  (define-key riece-dialogue-mode-map "\C-dr" nil))

(provide 'riece-ndcc)

;;; riece-ndcc.el ends here
