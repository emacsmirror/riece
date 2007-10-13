;;; riece-twitter.el --- post your status to Twitter
;; Copyright (C) 2007 Daiki Ueno

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

(require 'riece-message)

(defgroup riece-twitter nil
  "Post your status to Twitter"
  :group 'riece)
  
(defcustom riece-twitter-credential nil
  "Your credential used to login to Twitter."
  :group 'riece-twitter
  :type 'string)

(eval-and-compile
  (if (fboundp 'clear-string)
      (defalias 'riece-twitter-clear-string 'clear-string)
    (defun riece-twitter-clear-string (string)
      (fillarray string ?\0))))

(defun riece-twitter-set-credential (credential)
  "Set your credential used to login to Twitter."
  (interactive
   (let ((username (read-string "Username: "))
	 password)
     (unwind-protect
	 (list (concat username ":"
		       (setq password (read-passwd "Password: "))))
       (if password
	   (riece-twitter-clear-string password))
       (setq password nil))))
  (setq riece-twitter-credential credential))

(defun riece-twitter-update (status)
  "Update your status."
  (interactive
   (progn
     (unless riece-twitter-credential
       (error "%s"
	      (substitute-command-keys
	       "\\[riece-twitter-set-credential] to set your credential")))
     (list (read-string "Status: "))))
  (message "Sending to Twitter...")
  (let* ((args
	  (list "-u" riece-twitter-credential
		"-d" (concat "status="
			     (riece-twitter-escape-string
			      (encode-coding-string status 'utf-8)))
		"-s"
		"http://twitter.com/statuses/update.json"))
	 (process
	  (apply #'start-process
		 "curl" nil "curl"
		 (if (interactive-p)
		     args
		   (append args
			   (list "-H" "X-Twitter-Client: Riece"
				 "-H" (concat "X-Twitter-Client-Version: "
					      riece-version-number)
				 "-H" "X-Twitter-Client-URL: http://riece.nongnu.org/twitter.xml"
				 "-d" "source=riece"))))))
    (set-process-sentinel process #'riece-twitter-sentinel)))

(defun riece-twitter-sentinel (process status)
  (if (equal status "finished\n")
      (message "Sending to Twitter...done")
    (message "Sending to Twitter...failed: %s"
	     (substring status 0 (1- (length status))))))

(defun riece-twitter-message-filter (message)
  (if (and (riece-message-own-p message)
	   (eq 'action (riece-message-type message)))
      (if riece-twitter-credential
	  (riece-twitter-update (riece-message-text message))
	(message "%s"
		 (substitute-command-keys
		  "\\[riece-twitter-set-credential] to set your credential"))))
  message)

(defun riece-twitter-escape-string (string)
  (let ((index 0))
    (while (string-match "[^0-9A-Za-z\-\._~:/?@!\$'()*,]" string index)
      (setq string (replace-match
		    (format "%%%02X" (aref string (match-beginning 0)))
		    t t string)
	    index (+ 3 (match-beginning 0))))
    string))

(defun riece-twitter-insinuate ()
  (add-hook 'riece-message-filter-functions 'riece-twitter-message-filter))

(defun riece-twitter-uninstall ()
  (remove-hook 'riece-message-filter-functions 'riece-twitter-message-filter))

(provide 'riece-twitter)

;;; riece-twitter.el ends here
