;;; riece-300.el --- handlers for 300 replies
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

(require 'riece-misc)
(require 'riece-commands)

(require 'riece-filter)			;riece-default-handle-message

(eval-when-compile
  (autoload 'riece-default-handle-numeric-reply "riece-filter"))
(defun riece-handle-default-300-message (prefix number name string)
  (riece-default-handle-numeric-reply
   riece-info-prefix prefix number name string))

(defun riece-handle-302-message (prefix number name string)
  "RPL_USERHOST \":*1<reply> *( \" \" <reply> )\""
  (let ((replies (split-string (substring string 1) " ")))
    (while replies
      (if (string-match
	   (concat "^\\(" riece-user-regexp
		   "\\)\\(\\*\\)?=\\([-+]\\)\\([^ ]+\\)")
	   (car replies))
	  (riece-insert-info
	   (list riece-dialogue-buffer riece-others-buffer)
	   (concat
	    (riece-concat-server-name
	     (format "%s is (%s) [%s, %s]"
		     (match-string 1 (car replies))
		     (riece-strip-user-at-host (match-string 4 (car replies)))
		     (if (match-beginning 2)
			 "operator"
		       "not operator")
		     (if (eq (match-string 3 (car replies)) ?-)
			 "away"
		       "not away")))
	    "\n")))
      (setq replies (cdr replies)))))

(defun riece-handle-303-message (prefix number name string)
  (riece-insert-info
   (list riece-dialogue-buffer riece-others-buffer)
   (concat
    (riece-concat-server-name (concat "Online: " (substring string 1)))
    "\n")))

(defun riece-handle-301-message (prefix number name string)
  (if (string-match
       (concat "^\\(" riece-user-regexp "\\) :")
       string)
      (riece-insert-info
       (list riece-dialogue-buffer riece-others-buffer)
       (concat
	(riece-concat-server-name
	 (format "%s is away: %s"
		 (match-string 1 string)
		 (substring string (match-end 0))))
	"\n"))))

(defun riece-handle-311-message (prefix number name string)
  (if (string-match
       (concat "^\\(" riece-user-regexp
	       "\\) \\([^ ]+\\) \\([^ ]+\\) \\* :")
       string)
      (riece-insert-info
       (list riece-dialogue-buffer riece-others-buffer)
       (concat
	(riece-concat-server-name
	 (format "%s is %s (%s@%s)"
		 (match-string 1 string)
		 (substring string (match-end 0))
		 (match-string 2 string)
		 (match-string 3 string)))
	"\n"))))

(defun riece-handle-312-message (prefix number name string)
  (if (string-match
       (concat "^\\(" riece-user-regexp "\\) \\([^ ]+\\) :")
       string)
      (riece-insert-info
       (list riece-dialogue-buffer riece-others-buffer)
       (concat
	(riece-concat-server-name
	 (format "on via server %s: %s"
		 riece-real-server-name
		 (substring string (match-end 0))))
	"\n"))))

(defun riece-handle-313-message (prefix number name string)
  (if (string-match (concat "^" riece-user-regexp) string)
      (riece-insert-info
       (list riece-dialogue-buffer riece-others-buffer)
       (concat
	(riece-concat-server-name
	 (concat (match-string 0 string) " is an IRC operator"))
	"\n"))))

(defun riece-handle-317-message (prefix number name string)
  (if (string-match
       (concat "^\\(" riece-user-regexp "\\) \\([0-9]+\\) :")
       string)
      (riece-insert-info
       (list riece-dialogue-buffer riece-others-buffer)
       (concat
	(riece-concat-server-name
	 (format "%s is %s seconds idle"
		 (match-string 1 string)
		 (match-string 2 string)))
	"\n"))))

(defun riece-handle-318-message (prefix number name string))

(defun riece-handle-351-message (prefix number name string)
  (if (string-match "\\([^ ]+\\.[^ ]+\\) \\([^ ]+\\) :" string)
      (riece-insert-info
       (list riece-dialogue-buffer riece-others-buffer)
       (concat
	(riece-concat-server-name
	 (format "%s is running on %s: %s"
		 (match-string 1 string)
		 (match-string 2 string)
		 (substring string (match-end 0))))
	"\n"))))

(defun riece-handle-315-message (prefix number name string))

(defun riece-handle-353-message (prefix number name string)
  "RPL_NAMREPLY	\"<channel> :[[@|+]<nick> [[@|+]<nick> [...]]]\"."
  (if (string-match "^[=\*@] *\\([^ ]+\\) +:" string)
      (let ((channel (match-string 1 string))
	    users)
	(setq string (substring string (match-end 0)))
	(if (string-match " *$" string)
	    (setq string (substring string 0 (match-beginning 0))))
	(setq users (split-string string))
	(while users
	  (if (eq (aref (car users) 0) ?@)
	      (progn
		(riece-naming-assert-join (substring (car users) 1) channel)
		(riece-channel-toggle-operator
		 channel (substring (car users) 1) t))
	    (if (eq (aref (car users) 0) ?+)
		(progn
		  (riece-naming-assert-join (substring (car users) 1) channel)
		  (riece-channel-toggle-speaker
		   channel (substring (car users) 1) t))
	      (riece-naming-assert-join (car users) channel)))
	  (setq users (cdr users)))
	(riece-redisplay-buffers))))

(defun riece-handle-366-message (prefix number name string))

(defun riece-handle-324-message (prefix number name string)
  (if (string-match "^\\([^ ]+\\) \\([^ ]+\\) " string)
      (let* ((channel (match-string 1 string))
	     (mode-string (substring string (match-beginning 2)))
	     (modes (string-to-list (match-string 2 string)))
	     (toggle (pop modes)))
	(while modes
	  (riece-channel-toggle-mode channel (car modes) (eq toggle ?+))
	  (setq modes (cdr modes)))
	(let ((buffer (cdr (riece-identity-assoc-no-server
			    (riece-make-identity channel)
			    riece-channel-buffer-alist))))
	  (riece-insert-info buffer (concat "Mode: " mode-string "\n"))
	  (riece-insert-info
	   (if (and riece-channel-buffer-mode
		    (not (eq buffer riece-channel-buffer)))
	       (list riece-dialogue-buffer riece-others-buffer)
	     riece-dialogue-buffer)
	   (concat
	    (riece-concat-server-name
	     (format "Mode for %s: %s" channel mode-string))
	    "\n"))))))

(defun riece-handle-set-topic (prefix number name string remove)
  (if (string-match "^\\([^ ]+\\) :" string)
      (let* ((channel (match-string 1 string))
	     (message (substring string (match-end 0)))
	     (buffer (cdr (riece-identity-assoc-no-server
			   (riece-make-identity channel)
			   riece-channel-buffer-alist))))
	(if remove
	    (riece-channel-set-topic (riece-get-channel channel) nil)
	  (riece-channel-set-topic (riece-get-channel channel) message)
	(riece-insert-info buffer (concat "Topic: " message "\n"))
	(riece-insert-info
	 (if (and riece-channel-buffer-mode
		  (not (eq buffer riece-channel-buffer)))
	     (list riece-dialogue-buffer riece-others-buffer)
	   riece-dialogue-buffer)
	 (concat
	  (riece-concat-server-name
	   (format "Topic for %s: %s" channel message))
	  "\n"))
	(riece-update-channel-indicator)))))

(defun riece-handle-331-message (prefix number name string)
  (riece-handle-set-topic prefix name name string t))

(defun riece-handle-332-message (prefix number name string)
  (riece-handle-set-topic prefix name name string nil))

(provide 'riece-300)

;;; riece-300.el ends here
