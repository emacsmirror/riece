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

(require 'riece-misc)

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
	  (let ((user (match-string 1 (car replies)))
		(away (eq (match-string 3 (car replies)) ?-))
		(user-at-host (match-string 4 (car replies)))
		(operator (not (null (match-beginning 2)))))
	    (riece-user-toggle-away user away)
	    (riece-user-toggle-operator user operator)
	    (riece-insert-info
	     (list riece-dialogue-buffer riece-others-buffer)
	     (concat
	      (riece-concat-server-name
	       (format "%s is (%s) [%s, %s]"
		       (riece-format-identity
			(riece-make-identity user riece-server-name)
			t)
		       (riece-strip-user-at-host user-at-host)
		       (if operator
			   "operator"
			 "not operator")
		       (if away
			   "away"
			 "not away")))
	      "\n"))))
      (setq replies (cdr replies)))
  (riece-update-status-indicators)
  (force-mode-line-update t)))

(defun riece-handle-303-message (prefix number name string)
  (riece-insert-info
   (list riece-dialogue-buffer riece-others-buffer)
   (concat
    (riece-concat-server-name
     (concat "Online: "
	     (mapconcat
	      (lambda (user)
		(riece-format-identity
		 (riece-make-identity user riece-server-name)
		 t))
	      (split-string (substring string 1) " ")
	      "")))
    "\n")))

(defun riece-handle-301-message (prefix number name string)
  (if (string-match (concat "^\\(" riece-user-regexp "\\) :") string)
      (let ((user (match-string 1 string))
	    (message (substring string (match-end 0))))
	(riece-user-toggle-away user t)
	(riece-insert-info
	 (list riece-dialogue-buffer riece-others-buffer)
	 (concat
	  (riece-concat-server-name
	   (format "%s is away: %s"
		   (riece-format-identity
		    (riece-make-identity user riece-server-name)
		    t)
		   message))
	  "\n"))))
  (riece-update-status-indicators)
  (force-mode-line-update t))

(defun riece-handle-305-message (prefix number name string)
  (riece-user-toggle-away riece-real-nickname nil)
  (riece-update-status-indicators)
  (force-mode-line-update t))

(defun riece-handle-306-message (prefix number name string)
  (riece-user-toggle-away riece-real-nickname t)
  (riece-update-status-indicators)
  (force-mode-line-update t))

(defun riece-handle-311-message (prefix number name string)
  (if (string-match
       (concat "^\\(" riece-user-regexp
	       "\\) \\([^ ]+\\) \\([^ ]+\\) \\* :")
       string)
      (let ((user (match-string 1 string))
	    (name (substring string (match-end 0)))
	    (user-at-host (concat (match-string 2 string) "@"
				  (match-string 3 string))))
	(riece-insert-info
	 (list riece-dialogue-buffer riece-others-buffer)
	 (concat
	  (riece-concat-server-name
	   (format "%s is %s (%s)"
		   (riece-format-identity
		    (riece-make-identity user riece-server-name)
		    t)
		   name
		   user-at-host))
	  "\n")))))

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
      (let ((user (match-string 0 string)))
	(riece-insert-info
	 (list riece-dialogue-buffer riece-others-buffer)
	 (concat
	  (riece-concat-server-name
	   (concat (riece-format-identity
		    (riece-make-identity user riece-server-name)
		    t)
		   " is an IRC operator"))
	  "\n")))))

(defun riece-handle-317-message (prefix number name string)
  (if (string-match
       (concat "^\\(" riece-user-regexp "\\) \\([0-9]+\\) [^:]*:seconds")
       string)
      (let ((user (match-string 1 string))
	    (idle (match-string 2 string)))
	(riece-insert-info
	 (list riece-dialogue-buffer riece-others-buffer)
	 (concat
	  (riece-concat-server-name
	   (format "%s is %s seconds idle"
		   (riece-format-identity
		    (riece-make-identity user riece-server-name)
		    t)
		   idle))
	  "\n")))))

(defun riece-handle-319-message (prefix number name string)
  (if (string-match (concat "^\\(" riece-user-regexp "\\) :") string)
      (let ((user (match-string 1 string))
	    (channels
	     (mapconcat
	      (lambda (channel)
		(if (string-match
		     (concat "^\\([@+]?\\)\\(" riece-channel-regexp "\\)")
		     channel)
		    (concat
		     (match-string 1 channel)
		     (riece-format-identity
		      (riece-make-identity (match-string 2 channel)
					   riece-server-name)
		      t))))
	      (split-string (substring string (match-end 0)) " ")
	      " ")))
	(riece-insert-info
	 (list riece-dialogue-buffer riece-others-buffer)
	 (concat
	  (riece-concat-server-name
	   (format "%s: %s"
		   (riece-format-identity
		    (riece-make-identity user riece-server-name)
		    t)
		   channels))
	  "\n")))))

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

(defun riece-handle-322-message (prefix number name string)
  (if (string-match "^\\([^ ]+\\) \\([0-9]+\\) :" string)
      (let* ((channel (match-string 1 string))
	     (visible (match-string 2 string))
	     (topic (substring string (match-end 0))))
	(riece-channel-set-topic (riece-get-channel channel) topic)
	(let* ((channel-identity (riece-make-identity channel
						      riece-server-name))
	       (buffer (riece-channel-buffer channel-identity)))
	  (riece-insert-info buffer (concat visible " users, topic: "
					    topic "\n"))
	  (riece-insert-info
	   (if (and riece-channel-buffer-mode
		    (not (eq buffer riece-channel-buffer)))
	       (list riece-dialogue-buffer riece-others-buffer)
	     riece-dialogue-buffer)
	   (concat
	    (riece-concat-server-name
	     (format "%s: %s users, topic: %s"
		     (riece-format-identity channel-identity t) visible topic))
	    "\n"))))))

(defun riece-handle-324-message (prefix number name string)
  (if (string-match "^\\([^ ]+\\) \\([^ ]+\\) " string)
      (let* ((channel (match-string 1 string))
	     (mode-string (substring string (match-beginning 2)))
	     (modes (string-to-list (match-string 2 string)))
	     (toggle (car modes)))
	(setq modes (cdr modes))
	(while modes
	  (riece-channel-toggle-mode channel (car modes) (eq toggle ?+))
	  (setq modes (cdr modes)))
	(let* ((channel-identity (riece-make-identity channel
						      riece-server-name))
	       (buffer (riece-channel-buffer channel-identity)))
	  (riece-insert-info buffer (concat "Mode: " mode-string "\n"))
	  (riece-insert-info
	   (if (and riece-channel-buffer-mode
		    (not (eq buffer riece-channel-buffer)))
	       (list riece-dialogue-buffer riece-others-buffer)
	     riece-dialogue-buffer)
	   (concat
	    (riece-concat-server-name
	     (format "Mode for %s: %s"
		     (riece-format-identity channel-identity t)
		     mode-string))
	    "\n")))
	(riece-update-channel-indicator)
	(force-mode-line-update t))))

(defun riece-handle-set-topic (prefix number name string remove)
  (if (string-match "^\\([^ ]+\\) :" string)
      (let* ((channel (match-string 1 string))
	     (message (substring string (match-end 0)))
	     (channel-identity (riece-make-identity channel riece-server-name))
	     (buffer (riece-channel-buffer channel-identity)))
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
	   (format "Topic for %s: %s"
		   (riece-format-identity channel-identity t)
		   message))
	  "\n"))
	(riece-update-channel-indicator)))))

(defun riece-handle-331-message (prefix number name string)
  (riece-handle-set-topic prefix number name string t))

(defun riece-handle-332-message (prefix number name string)
  (riece-handle-set-topic prefix number name string nil))

(defun riece-handle-341-message (prefix number name string)
  (if (string-match "^\\([^ ]+\\) " string)
      (let* ((channel (match-string 1 string))
	     (user (substring string (match-end 0)))
	     (channel-identity (riece-make-identity channel riece-server-name))
	     (buffer (riece-channel-buffer channel-identity)))
	(riece-insert-info buffer (concat "Inviting " user "\n"))
	(riece-insert-info
	 (if (and riece-channel-buffer-mode
		  (not (eq buffer riece-channel-buffer)))
	     (list riece-dialogue-buffer riece-others-buffer)
	   riece-dialogue-buffer)
	 (concat
	  (riece-concat-server-name
	   (format "Inviting %s to %s" user
		   (riece-format-identity channel-identity t)))
	  "\n")))))

(defun riece-handle-352-message (prefix number name string)
  (if (string-match "^\\([^ ]+\\) \\([^ ]+\\) \\([^ ]+\\) \\([^ ]+\\) \\([^ ]+\\) \\([HG]\\)\\(\\*\\)?\\([@+]\\)? :\\([0-9]+\\) " string)
      (let* ((channel (match-string 1 string))
	     (user (match-string 2 string))
	     (host (match-string 3 string))
	     (server (match-string 4 string))
	     (nick (match-string 5 string))
	     (away (equal (match-string 6 string) "G"))
	     (operator (not (null (match-beginning 7))))
	     (flag (match-string 8 string))
	     (hops (match-string 9 string))
	     (name (substring string (match-end 0)))
	     (buffer (riece-channel-buffer (riece-make-identity
					    channel riece-server-name)))
	     (info (format "%10s = %s (%s) [%s, %s, %s hops, on %s]"
			   (concat
			    (if (memq flag '(?@ ?+))
				(char-to-string flag)
			      " ")
			    (riece-format-identity
			     (riece-make-identity nick riece-server-name)
			     t))
			   name
			   (riece-strip-user-at-host
			    (concat user "@" host))
			   (if operator
			       "operator"
			     "not operator")
			   (if away
			       "away"
			     "not away")
			   hops
			   server)))
	(riece-naming-assert-join nick channel)
	(riece-user-toggle-away user away)
	(riece-user-toggle-operator user operator)
	(riece-insert-info buffer (concat info "\n"))
	(riece-insert-info
	 (if (and riece-channel-buffer-mode
		  (not (eq buffer riece-channel-buffer)))
	     (list riece-dialogue-buffer riece-others-buffer)
	   riece-dialogue-buffer)
	 (concat
	  (riece-concat-server-name
	   (concat
	    (riece-format-identity
	     (riece-make-identity channel riece-server-name)
	     t)
	    " "
	    info))
	  "\n"))
	(riece-redisplay-buffers))))

(defun riece-handle-315-message (prefix number name string))
(defun riece-handle-318-message (prefix number name string))
(defun riece-handle-323-message (prefix number name string))
(defun riece-handle-366-message (prefix number name string))

(provide 'riece-300)

;;; riece-300.el ends here
