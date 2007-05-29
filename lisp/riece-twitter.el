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

(defcustom riece-twitter-cache-credential t
  "If non-nil, cache your credential on Twitter."
  :group 'riece-twitter
  :type 'boolean)

(defun riece-twitter-message-filter (message)
  (if (and (riece-message-own-p message)
	   (eq 'action (riece-message-type message)))
      (let ((credential
	     (or riece-twitter-credential
		 (concat (read-string "Twitter username: ") ":"
			 (read-passwd "Twitter password: ")))))
	(if (and riece-twitter-cache-credential
		 (not (eq credential riece-twitter-credential)))
	    (setq riece-twitter-credential credential))
	(start-process
	 "curl" nil "curl"
	 "-H" "X-Twitter-Client: Riece"
	 "-H" (concat "X-Twitter-Client-Version: " riece-version-number)
	 "-H" "X-Twitter-Client-URL: http://riece.nongnu.org/twitter.xml"
	 "-u" credential
	 "-d" (concat "status="
		      (riece-twitter-escape-string
		       (encode-coding-string (riece-message-text message)
					     'utf-8)))
	 "-s"
	 "http://twitter.com/statuses/update.json")))
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
