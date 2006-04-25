(require 'riece-message)
(require 'riece-coding)

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
      (let ((entry (assoc identity riece-epg-passphrase-alist))
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

(defun riece-command-enter-encrypted-message ()
  "Encrypt the current line send send it to the current channel."
  (interactive)
  (let ((context (epg-make-context))
	(string (riece-encode-coding-string
		 (buffer-substring
		  (riece-line-beginning-position)
		  (riece-line-end-position))))
	entry)
    (epg-context-set-passphrase-callback
     context
     (cons #'riece-epg-passphrase-callback-function
	   riece-current-channel))
    (condition-case error
	(setq string (epg-encrypt-string context string nil))
      (error
       (if (setq entry (assoc riece-current-channel
			      riece-epg-passphrase-alist))
	   (setcdr entry nil))
       (signal (car error) (cdr error))))
    (riece-command-send-message
     (concat "[OpenPGP Encrypted:" (base64-encode-string string t) "]")
     nil)
    (let ((next-line-add-newlines t))
      (next-line 1))))

(defun riece-epg-message-filter (message)
  (if (get 'riece-epg 'riece-addon-enabled)
      (when (string-match "\\`\\[OpenPGP Encrypted:\\(.*\\)]"
			  (riece-message-text message))
	(let ((context (epg-make-context))
	      (string (match-string 1 (riece-message-text message)))
	      entry)
	  (epg-context-set-passphrase-callback
	   context
	   (cons #'riece-epg-passphrase-callback-function
		 (riece-message-target message)))
	  (condition-case error
	      (setq string (epg-decrypt-string
			    context
			    (base64-decode-string string)))
	    (error
	     (if (setq entry (assoc (riece-message-target message)
				    riece-epg-passphrase-alist))
		 (setcdr entry nil))
	     (message "%s" (cdr error))))
	  (riece-message-set-text message
				  (riece-decode-coding-string string)))))
  message)

(defun riece-epg-insinuate ()
  (add-hook 'riece-message-filter-functions 'riece-epg-message-filter))

(defun riece-epg-uninstall ()
  (remove-hook 'riece-message-filter-functions 'riece-epg-message-filter))

(defvar riece-command-mode-map)
(defun riece-epg-enable ()
  (define-key riece-command-mode-map
    "\C-ce" 'riece-command-enter-encrypted-message))

(defun riece-epg-disable ()
  (define-key riece-command-mode-map
    "\C-ce" nil))

(provide 'riece-epg)

;;; riece-epg.el ends here
