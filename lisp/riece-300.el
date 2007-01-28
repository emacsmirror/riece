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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'riece-misc)
(require 'riece-naming)
(require 'riece-signal)
(require 'riece-display)

(eval-when-compile
  (autoload 'riece-default-handle-numeric-reply "riece-handle"))
(defun riece-handle-default-300-message (prefix number name string)
  (riece-default-handle-numeric-reply
   riece-info-prefix prefix number name string))

(defun riece-handle-302-message (prefix number name string)
  "RPL_USERHOST \":*1<reply> *( \" \" <reply> )\""
  (let ((replies (split-string (if (eq (aref string 0) ?:)
				   (substring string 1)
				 string)
			       " ")))
    (while replies
      (if (string-match
	   (concat "^\\([^ ]+\\)\\(\\*\\)?=\\([-+]\\)\\([^ ]+\\)")
	   (car replies))
	  (let ((user (match-string 1 (car replies)))
		(operator (not (null (match-beginning 2))))
		(away (eq (match-string 3 (car replies)) ?-))
		(user-at-host (match-string 4 (car replies)))
		status)
	    (if away
		(setq status (cons "away" status)))
	    (if operator
		(setq status (cons "operator" status)))
	    (riece-user-toggle-away user away)
	    (riece-emit-signal 'user-away-changed
			       (riece-make-identity user riece-server-name)
			       away)
	    (riece-user-toggle-operator user operator)
	    (riece-emit-signal 'user-operator-changed
			       (riece-make-identity user riece-server-name)
			       operator)
	    (riece-insert-info
	     (list riece-dialogue-buffer riece-others-buffer)
	     (concat
	      (riece-concat-server-name
	       (riece-concat-user-status
		status
		(format (riece-mcat "%s is (%s)")
			(riece-format-identity
			 (riece-make-identity user riece-server-name)
			 t)
			(riece-strip-user-at-host user-at-host))))
	      "\n"))))
      (setq replies (cdr replies)))))

(defun riece-handle-303-message (prefix number name string)
  (riece-insert-info
   (list riece-dialogue-buffer riece-others-buffer)
   (concat
    (riece-concat-server-name
     (concat (riece-mcat "Online: ")
	     (mapconcat
	      (lambda (user)
		(riece-format-identity
		 (riece-make-identity user riece-server-name)
		 t))
	      (split-string (if (eq (aref string 0) ?:)
				(substring string 1)
			      string)
			    " ")
	      "")))
    "\n")))

(defun riece-handle-301-message (prefix number name string)
  (if (string-match (concat "^\\([^ ]+\\) :?") string)
      (let ((user (match-string 1 string))
	    (message (substring string (match-end 0))))
	(riece-user-toggle-away user t)
	(riece-emit-signal 'user-away-changed
			   (riece-make-identity user riece-server-name)
			   t)
	(riece-insert-info
	 (list riece-dialogue-buffer riece-others-buffer)
	 (concat
	  (riece-concat-server-name
	   (format (riece-mcat "%s is away: %s")
		   (riece-format-identity
		    (riece-make-identity user riece-server-name)
		    t)
		   message))
	  "\n")))))

(defun riece-handle-305-message (prefix number name string)
  (riece-user-toggle-away riece-real-nickname nil)
  (riece-emit-signal 'user-away-changed
		      (riece-make-identity riece-real-nickname
					   riece-server-name)
		      nil))

(defun riece-handle-306-message (prefix number name string)
  (riece-user-toggle-away riece-real-nickname t)
  (riece-emit-signal 'user-away-changed
		     (riece-make-identity riece-real-nickname
					  riece-server-name)
		     t))

(defun riece-handle-311-message (prefix number name string)
  (if (string-match
       (concat "^\\([^ ]+\\) \\([^ ]+\\) \\([^ ]+\\) \\* :?")
       string)
      (let ((user (match-string 1 string))
	    (name (substring string (match-end 0)))
	    (user-at-host (concat (match-string 2 string) "@"
				  (match-string 3 string))))
	(riece-insert-info
	 (list riece-dialogue-buffer riece-others-buffer)
	 (concat
	  (riece-concat-server-name
	   (format (riece-mcat "%s is %s (%s)")
		   (riece-format-identity
		    (riece-make-identity user riece-server-name)
		    t)
		   name
		   user-at-host))
	  "\n")))))

(defun riece-handle-312-message (prefix number name string)
  (if (string-match
       (concat "^\\([^ ]+\\) \\([^ ]+\\) :?")
       string)
      (riece-insert-info
       (list riece-dialogue-buffer riece-others-buffer)
       (concat
	(riece-concat-server-name
	 (format (riece-mcat "on via server %s: %s")
		 (match-string 2 string)
		 (substring string (match-end 0))))
	"\n"))))

(defun riece-handle-313-message (prefix number name string)
  (if (string-match "^[^ ]+" string)
      (let ((user (match-string 0 string)))
	(riece-insert-info
	 (list riece-dialogue-buffer riece-others-buffer)
	 (concat
	  (riece-concat-server-name
	   (format "%s is an IRC operator"
		   (riece-format-identity
		    (riece-make-identity user riece-server-name)
		    t)))
	  "\n")))))

(defun riece-handle-317-message (prefix number name string)
  (if (string-match
       (concat "^\\([^ ]+\\) \\([0-9]+\\) ")
       string)
      (let* ((user (match-string 1 string))
	     (seconds (string-to-number (match-string 2 string)))
	     (units (list (cons (/ seconds 60 60 24) (riece-mcat "days"))
			  (cons (mod (/ seconds 60 60) 24)
				(riece-mcat "hours"))
			  (cons (mod (/ seconds 60) 60) (riece-mcat "minutes"))
			  (cons (mod seconds 60) (riece-mcat "seconds")))))
	(riece-insert-info
	 (list riece-dialogue-buffer riece-others-buffer)
	 (concat
	  (riece-concat-server-name
	   (format (riece-mcat "%s is %s idle")
		   (riece-format-identity
		    (riece-make-identity user riece-server-name)
		    t)
		   (mapconcat #'identity
			      (delq nil
				    (mapcar
				     (lambda (unit)
				       (if (/= (car unit) 0)
					   (format "%d %s"
						   (car unit) (cdr unit))))
				     units))
			      " ")))
	  "\n")))))

(defun riece-handle-319-message (prefix number name string)
  (if (string-match (concat "^\\([^ ]+\\) :?") string)
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
  (if (string-match "\\([^ ]+\\.[^ ]+\\) \\([^ ]+\\) :?" string)
      (riece-insert-info
       (list riece-dialogue-buffer riece-others-buffer)
       (concat
	(riece-concat-server-name
	 (format (riece-mcat "%s is running on %s: %s")
		 (match-string 1 string)
		 (match-string 2 string)
		 (substring string (match-end 0))))
	"\n"))))

(defvar riece-353-message-alist nil)
(defun riece-handle-353-message (prefix number name string)
  "RPL_NAMREPLY	\"[=\*@] <channel> :[[@|+]<nick> [[@|+]<nick> [...]]]\"."
  (make-local-variable 'riece-353-message-alist)      
  (if (string-match "^[=\*@] *\\([^ ]+\\) +:?" string)
      (let* ((channel (match-string 1 string))
	     (entry (riece-identity-assoc channel riece-353-message-alist t)))
	(if entry
	    (setcdr entry
		    (concat (cdr entry)
			    (substring string (match-end 0)) " "))
	  (setq riece-353-message-alist
		(cons (cons channel
			    (concat (substring string (match-end 0)) " "))
		      riece-353-message-alist))))))

(defun riece-handle-322-message (prefix number name decoded)
  (let* ((parameters (riece-split-parameters (riece-decoded-string decoded)))
	 (channel (car parameters))
	 (visible (nth 1 parameters))
	 (channel-identity (riece-make-identity channel riece-server-name))
	 (buffer (riece-channel-buffer channel-identity))
	 topic)
    (setq parameters (riece-split-parameters
		      (riece-decoded-string-for-identity decoded
							 channel-identity))
	  topic (nth 2 parameters))
    (riece-channel-set-topic (riece-get-channel channel) topic)
    (riece-insert-info buffer (format (riece-mcat "%s users, topic: %s\n")
				      visible topic))
    (riece-insert-info
     (if (and riece-channel-buffer-mode
	      (not (eq buffer riece-channel-buffer)))
	 (list riece-dialogue-buffer riece-others-buffer)
       riece-dialogue-buffer)
     (concat
      (riece-concat-server-name
       (format (riece-mcat "%s: %s users, topic: %s")
	       (riece-format-identity channel-identity t) visible topic))
      "\n"))))

(defun riece-handle-324-message (prefix number name string)
  (if (string-match "^\\([^ ]+\\) \\([^ ]+\\) " string)
      (let* ((channel (match-string 1 string))
	     (mode-string (match-string 2 string)))
	(riece-naming-assert-channel-modes channel
					   (riece-parse-modes mode-string))
	(let* ((channel-identity (riece-make-identity channel
						      riece-server-name))
	       (buffer (riece-channel-buffer channel-identity)))
	  (riece-insert-info buffer (concat (riece-mcat "Mode: ") mode-string
					    "\n"))
	  (riece-insert-info
	   (if (and riece-channel-buffer-mode
		    (not (eq buffer riece-channel-buffer)))
	       (list riece-dialogue-buffer riece-others-buffer)
	     riece-dialogue-buffer)
	   (concat
	    (riece-concat-server-name
	     (format (riece-mcat "Mode for %s: %s")
		     (riece-format-identity channel-identity t)
		     mode-string))
	    "\n"))))))

(defun riece-handle-set-topic (prefix number name decoded remove)
  (let* ((parameters (riece-split-parameters (riece-decoded-string decoded)))
	 (channel (car parameters))
	 topic
	 (channel-identity (riece-make-identity channel riece-server-name))
	 (buffer (riece-channel-buffer channel-identity)))
    (if remove
	(riece-channel-set-topic (riece-get-channel channel) nil)
      (setq parameters (riece-split-parameters
			(riece-decoded-string-for-identity decoded
							   channel-identity))
	    topic (nth 1 parameters))
      (riece-channel-set-topic (riece-get-channel channel) topic)
      (riece-insert-info buffer (concat (riece-mcat "Topic: ") topic "\n"))
      (riece-insert-info
       (if (and riece-channel-buffer-mode
		(not (eq buffer riece-channel-buffer)))
	   (list riece-dialogue-buffer riece-others-buffer)
	 riece-dialogue-buffer)
       (concat
	(riece-concat-server-name
	 (format (riece-mcat "Topic for %s: %s")
		 (riece-format-identity channel-identity t)
		 topic))
	"\n")))
    (riece-emit-signal 'channel-topic-changed channel-identity topic)))

(defun riece-handle-331-message (prefix number name string)
  (riece-handle-set-topic prefix number name string t))

(defun riece-handle-332-message (prefix number name string)
  (riece-handle-set-topic prefix number name string nil))

(defun riece-handle-341-message (prefix number name string)
  (if (string-match "^\\([^ ]+\\) " string)
      (let* ((channel (substring string (match-end 0)))
	     (user (match-string 1 string))
	     (channel-identity (riece-make-identity channel riece-server-name))
	     (buffer (riece-channel-buffer channel-identity)))
	(riece-insert-info buffer (format (riece-mcat "Inviting %s\n") user))
	(riece-insert-info
	 (if (and riece-channel-buffer-mode
		  (not (eq buffer riece-channel-buffer)))
	     (list riece-dialogue-buffer riece-others-buffer)
	   riece-dialogue-buffer)
	 (concat
	  (riece-concat-server-name
	   (format (riece-mcat "Inviting %s to %s") user
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
	     (info (format "%10s = %s (%s)"
			   (concat
			    (if (memq flag '(?@ ?+))
				(char-to-string flag)
			      " ")
			    (riece-format-identity
			     (riece-make-identity nick riece-server-name)
			     t))
			   name
			   (riece-strip-user-at-host
			    (concat user "@" host))))
	     status)
	(if operator
	    (setq status (cons "operator" status)))
	(if away
	    (setq status (cons "away" status)))
	(unless (equal hops "0")
	  (setq status (cons (concat "on " server)
			     (cons (concat hops " hops")
				   status))))
	(if status
	    (setq status (nreverse status)))
	(riece-naming-assert-join nick channel)
	(riece-user-toggle-away user away)
	(riece-emit-signal 'user-away-changed
			   (riece-make-identity user riece-server-name)
			   away)
	(riece-user-toggle-operator user operator)
	(riece-emit-signal 'user-operator-changed
			   (riece-make-identity user riece-server-name)
			   operator)
	(riece-insert-info buffer (concat (riece-concat-user-status
					   status info)
					  "\n"))
	(riece-insert-info
	 (if (and riece-channel-buffer-mode
		  (not (eq buffer riece-channel-buffer)))
	     (list riece-dialogue-buffer riece-others-buffer)
	   riece-dialogue-buffer)
	 (concat
	  (riece-concat-server-name
	   (riece-concat-user-status
	    status
	    (concat
	     (riece-format-identity
	      (riece-make-identity channel riece-server-name)
	      t)
	     " "
	     info)))
	  "\n")))))

(defun riece-handle-315-message (prefix number name string))
(defun riece-handle-318-message (prefix number name string))
(defun riece-handle-323-message (prefix number name string))

(defun riece-handle-366-message (prefix number name string)
  "RPL_ENDOFNAMES \"<channel> :End of NAMES list\""
  (if (string-match "^\\([^ ]+\\) " string)
      (let* ((channel (match-string 1 string))
	     (channel-identity (riece-make-identity channel
						    riece-server-name))
	     (buffer (riece-channel-buffer channel-identity))
	     (entry (riece-identity-assoc channel riece-353-message-alist t))
	     (string (cdr entry))
	     (start 0)
	     users)
	(if entry
	    (setq riece-353-message-alist
		  (delq entry riece-353-message-alist)))
	(while (string-match
		(concat "\\([@+]\\)?\\([^ ]+\\) +")
		string start)
	  (put-text-property (match-beginning 2) (match-end 2)
			     'riece-identity
			     (riece-make-identity (match-string 2 string)
						  riece-server-name)
			     string)
	  (setq start (match-end 0)
		users (cons (if (match-beginning 1)
				(if (eq (aref string (match-beginning 1)) ?@)
				    (list (match-string 2 string) ?o)
				  (if (eq (aref string (match-beginning 1)) ?+)
				      (list (match-string 2 string) ?v)))
			      (list (match-string 2 string)))
			    users)))
	(setq users (nreverse users))
	(riece-naming-assert-channel-users users channel)
	(riece-insert-info
	 buffer
	 (concat (format (riece-mcat "%d users: ") (length users)) string
		 "\n"))
	(riece-insert-info
	 (if (and riece-channel-buffer-mode
		  (not (eq buffer riece-channel-buffer)))
	     (list riece-dialogue-buffer riece-others-buffer)
	   riece-dialogue-buffer)
	 (concat
	  (riece-concat-server-name
	   (concat (format (riece-mcat "%d users on %s: ")
			   (length users)
			   (riece-format-identity channel-identity t))
		   string))
	  "\n")))))

(provide 'riece-300)

;;; riece-300.el ends here
