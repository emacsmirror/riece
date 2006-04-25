;;; riece-epg.el --- Encrypt/decrypt messages add-on
;; Copyright (C) 2006 Daiki Ueno

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

;;; Code:

(require 'riece-message)
(require 'riece-identity)

(autoload 'epg-make-context "epg")
(autoload 'epg-decrypt-string "epg")
(autoload 'epg-encrypt-string "epg")
(autoload 'epg-passphrase-callback-function "epg")
(autoload 'epg-context-set-passphrase-callback "epg")

(eval-when-compile
  (autoload 'riece-command-send-message "riece-commands"))

(defgroup riece-epg nil
  "Encrypt/decrypt messages."
  :group 'riece)

(defconst riece-epg-description
  "Encrypt/decrypt messages.")

(defvar riece-epg-passphrase-alist nil)

(defun riece-epg-passphrase-callback-function (key-id identity)
  (if (eq key-id 'SYM)
      (let ((entry (riece-identity-assoc identity riece-epg-passphrase-alist))
	    passphrase)
	(or (copy-sequence (cdr entry))
	    (progn
	      (unless entry
		(setq entry (list identity)
		      riece-epg-passphrase-alist (cons entry
						 riece-epg-passphrase-alist)))
	      (setq passphrase (epg-passphrase-callback-function key-id nil))
	      (setcdr entry (copy-sequence passphrase))
	      passphrase)))
    (epg-passphrase-callback-function key-id nil)))

(defun riece-epg-funcall-clear-passphrase (identity function &rest args)
  (condition-case error
      (apply function args)
    (error
     (let ((entry (riece-identity-assoc identity riece-epg-passphrase-alist)))
       (if entry
	   (setq riece-epg-passphrase-alist
		 (delq entry riece-epg-passphrase-alist))))
     (signal (car error) (cdr error)))))
  
(defun riece-command-enter-encrypted-message ()
  "Encrypt the current line send send it to the current channel."
  (interactive)
  (let ((context (epg-make-context))
	(string (buffer-substring
		 (riece-line-beginning-position)
		 (riece-line-end-position)))
	entry)
    (riece-with-server-buffer (riece-identity-server riece-current-channel)
      (setq string (riece-encode-coding-string-for-identity
		    string
		    riece-current-channel)))
    (epg-context-set-passphrase-callback
     context
     (cons #'riece-epg-passphrase-callback-function
	   riece-current-channel))
    (setq string (riece-epg-funcall-clear-passphrase riece-current-channel
						     #'epg-encrypt-string
						     context string nil))
    (riece-command-send-message
     (concat "[encrypted:" (base64-encode-string string t) "]")
     nil)
    (let ((next-line-add-newlines t))
      (next-line 1))))

(defun riece-command-change-passphrase (identity passphrase)
  "Change PASSPHRASE associated with IDENTITY."
  (interactive
   (let ((identity
	  (riece-completing-read-identity
	   "Channel/user: "
	   riece-current-channels nil t nil nil
	   (riece-format-identity riece-current-channel))))
     (list identity
	   (read-passwd (format "Passphrase for %s: "
				(riece-format-identity identity))))))
  (let ((entry (riece-identity-assoc identity riece-epg-passphrase-alist)))
    (if (equal passphrase "")
	(if entry
	    (setq riece-epg-passphrase-alist
		  (delq entry riece-epg-passphrase-alist)))
      (if entry
	  (setcdr entry passphrase)
	(setq riece-epg-passphrase-alist
	      (cons (cons identity passphrase)
		    riece-epg-passphrase-alist))))))

(defun riece-epg-message-filter (message)
  (if (get 'riece-epg 'riece-addon-enabled)
      (when (string-match "\\`\\[encrypted:\\(.*\\)]"
			  (riece-message-text message))
	(let ((context (epg-make-context))
	      (string (match-string 1 (riece-message-text message)))
	      (coding-system (or (riece-coding-system-for-identity
				  (riece-message-target message))
				 riece-default-coding-system))
	      entry)
	  (epg-context-set-passphrase-callback
	   context
	   (cons #'riece-epg-passphrase-callback-function
		 (riece-message-target message)))
	  (condition-case error
	      (progn
		(setq string (base64-decode-string string))
		(riece-message-set-text
		 message
		 (concat
		  "[decrypted:"
		  (riece-with-server-buffer
		      (riece-identity-server (riece-message-target message))
		    (decode-coding-string
		     (riece-epg-funcall-clear-passphrase
		      (riece-message-target message)
		      #'epg-decrypt-string context string)
		     (if (consp coding-system)
			 (car coding-system)
		       coding-system)))
		  "]")))
	    (error (message "%s" (cdr error)))))))
  message)

(defun riece-epg-insinuate ()
  (add-hook 'riece-message-filter-functions 'riece-epg-message-filter))

(defun riece-epg-uninstall ()
  (remove-hook 'riece-message-filter-functions 'riece-epg-message-filter))

(defvar riece-command-mode-map)
(defun riece-epg-enable ()
  (define-key riece-command-mode-map
    "\C-ce" 'riece-command-enter-encrypted-message)
  (define-key riece-command-mode-map
    "\C-c\C-ec" 'riece-command-change-passphrase))

(defun riece-epg-disable ()
  (define-key riece-command-mode-map
    "\C-ce" nil)
  (define-key riece-command-mode-map
    "\C-c\C-ec" nil))

(provide 'riece-epg)

;;; riece-epg.el ends here
