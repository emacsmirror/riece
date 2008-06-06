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

(autoload 'widget-convert-button "wid-edit")
(autoload 'epg-make-context "epg")
(autoload 'epg-decrypt-string "epg")
(autoload 'epg-encrypt-string "epg")
(autoload 'epg-passphrase-callback-function "epg")
(autoload 'epg-context-set-passphrase-callback "epg")
(autoload 'epg-cancel "epg")

(eval-when-compile
  (autoload 'riece-command-send-message "riece-commands"))

(defgroup riece-epg nil
  "Encrypt/decrypt messages."
  :group 'riece)

(defconst riece-epg-description
  "Encrypt/decrypt messages.")

(defvar riece-epg-passphrase-alist nil)

(defun riece-epg-passphrase-callback-function (context key-id identity)
  (if (eq key-id 'SYM)
      (let ((entry (riece-identity-assoc identity riece-epg-passphrase-alist))
	    passphrase)
	(or (copy-sequence (cdr entry))
	    (progn
	      (unless entry
		(setq entry (list identity)
		      riece-epg-passphrase-alist (cons entry
						 riece-epg-passphrase-alist)))
	      (setq passphrase (epg-passphrase-callback-function context
								 key-id nil))
	      (setcdr entry (copy-sequence passphrase))
	      passphrase)))
    (epg-passphrase-callback-function context key-id nil)))

(defun riece-epg-passphrase-callback-function-for-decrypt (context key-id
								   identity)
  (if (eq key-id 'SYM)
      (let ((entry (riece-identity-assoc identity riece-epg-passphrase-alist)))
	(if (cdr entry)
	    (copy-sequence (cdr entry))
	  (epg-cancel context)))
    (epg-passphrase-callback-function context key-id nil)))

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
  "Encrypt the current line and send it to the current channel."
  (interactive)
  (let ((context (epg-make-context))
	(string (buffer-substring (riece-line-beginning-position)
				  (riece-line-end-position))))
    (epg-context-set-passphrase-callback
     context
     (cons #'riece-epg-passphrase-callback-function
	   riece-current-channel))
    (riece-send-string
     (format "PRIVMSG %s :[encrypted:%s]\r\n"
	     (riece-identity-prefix riece-current-channel)
	     (base64-encode-string
	      (riece-epg-funcall-clear-passphrase
	       riece-current-channel
	       #'epg-encrypt-string
	       context
	       (riece-with-server-buffer
		   (riece-identity-server riece-current-channel)
		 (riece-encode-coding-string-for-identity
		  string
		  riece-current-channel))
	       nil)
	      t)))
    (riece-display-message
     (riece-make-message (riece-current-nickname) riece-current-channel
			 (concat "[encrypted:" string "]") nil t))
    (if (> (forward-line) 0)
	(insert "\n"))))

(defun riece-command-set-passphrase (identity passphrase)
  "Set PASSPHRASE associated with IDENTITY."
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

(defun riece-epg-decrypt-string-for-identity (context cipher target)
  (let ((coding-system
	 (or (riece-coding-system-for-identity target)
	     riece-default-coding-system)))
    (riece-with-server-buffer (riece-identity-server target)
      (decode-coding-string
       (riece-epg-funcall-clear-passphrase
	target
	#'epg-decrypt-string
	context
	(base64-decode-string cipher))
       (if (consp coding-system)
	   (car coding-system)
	 coding-system)))))

(defun riece-epg-message-filter (message)
  (if (get 'riece-epg 'riece-addon-enabled)
      (when (string-match "\\`\\[encrypted:\\(.*\\)]"
			  (riece-message-text message))
	(let ((context (epg-make-context))
	      (string (match-string 1 (riece-message-text message))))
	  (epg-context-set-passphrase-callback
	   context
	   (cons #'riece-epg-passphrase-callback-function-for-decrypt
		 riece-current-channel))
	  (condition-case error
	      (progn
		(riece-message-set-text
		 message
		 (format "[encrypted:%s]"
			 (riece-epg-decrypt-string-for-identity
			  context string (riece-message-target message)))))
	    (error
	     (riece-put-text-property-nonsticky
	      0 (length (riece-message-text message))
	      'riece-epg-encryption-target (riece-message-target message)
	      (riece-message-text message))
	     (if riece-debug
		 (message "Couldn't decrypt: %s" (cdr error))
	       (message "Couldn't decrypt")))))))
  message)

(defun riece-epg-add-encrypted-button (start end)
  (if (and (get 'riece-button 'riece-addon-enabled)
	   (get 'riece-epg 'riece-addon-enabled))
      (riece-scan-property-region
       'riece-epg-encryption-target
       start end
       (lambda (start end)
	 (let ((inhibit-read-only t)
	       buffer-read-only)
	   (widget-convert-button
	    'link start end
	    :help-echo "Click to decrypt"
	    :notify #'riece-epg-encrypted-button-notify
	    (get-text-property start 'riece-epg-encryption-target)))))))

(defun riece-epg-encrypted-button-notify (widget &rest ignore)
  (let* ((from (marker-position (widget-get widget :from)))
	 (to (marker-position (widget-get widget :to)))
	 (target (widget-get widget :value))
	 (cipher (buffer-substring from to))
	 (inhibit-read-only t)
	 buffer-read-only
	 plain)
    (when (string-match "\\`\\[encrypted:\\(.*\\)]" cipher)
      (setq plain (riece-epg-decrypt-string-for-identity
		   (epg-make-context) (match-string 1 cipher) target))
      (widget-delete widget)
      (delete-region from to)
      (save-excursion
	(goto-char from)
	(insert "[encrypted:" plain "]")))))

(defun riece-epg-requires ()
  (if (memq 'riece-button riece-addons)
      '(riece-button)))

(defun riece-epg-insinuate ()
  (add-hook 'riece-message-filter-functions 'riece-epg-message-filter)
  (add-hook 'riece-after-insert-functions 'riece-epg-add-encrypted-button))

(defun riece-epg-uninstall ()
  (remove-hook 'riece-message-filter-functions 'riece-epg-message-filter)
  (remove-hook 'riece-after-insert-functions 'riece-epg-add-encrypted-button))

(defvar riece-command-mode-map)
(defun riece-epg-enable ()
  (define-key riece-command-mode-map
    "\C-ce" 'riece-command-enter-encrypted-message)
  (define-key riece-command-mode-map
    "\C-c\C-ec" 'riece-command-set-passphrase))

(defun riece-epg-disable ()
  (define-key riece-command-mode-map
    "\C-ce" nil)
  (define-key riece-command-mode-map
    "\C-c\C-ec" nil))

(provide 'riece-epg)

;;; riece-epg.el ends here
