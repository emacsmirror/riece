;;; riece-handle.el --- basic message handlers
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
(require 'riece-message)
(require 'riece-channel)
(require 'riece-naming)
(require 'riece-display)

(defun riece-handle-nick-message (prefix string)
  (let* ((old (riece-prefix-nickname prefix))
	 (new (car (riece-split-parameters string)))
	 (old-identity (riece-make-identity old riece-server-name))
	 (new-identity (riece-make-identity new riece-server-name))
	 (channels (riece-user-get-channels old))
	 (visible (riece-identity-member
		   riece-current-channel
		   (mapcar (lambda (channel)
			     (riece-make-identity channel riece-server-name))
			   channels))))
    (riece-naming-assert-rename old new)
    (let ((pointer (riece-identity-member old-identity
					  riece-current-channels)))
      (when pointer
	(setcar pointer new-identity)
	(with-current-buffer (riece-channel-buffer-name new-identity)
	  (rename-buffer (riece-channel-buffer-name new-identity)))
	(if (riece-identity-equal new-identity riece-current-channel)
	    (riece-switch-to-channel new-identity))
	(setq channels (cons new-identity channels))))
    (riece-insert-change (mapcar
			  (lambda (channel)
			    (riece-channel-buffer-name
			     (riece-make-identity channel riece-server-name)))
			  channels)
			 (format "%s -> %s\n"
				 (riece-format-identity old-identity t)
				 (riece-format-identity new-identity t)))
    (riece-insert-change (if visible
			     riece-dialogue-buffer
			   (list riece-dialogue-buffer riece-others-buffer))
			 (concat
			  (riece-concat-server-name
			   (format "%s -> %s"
				 (riece-format-identity old-identity t)
				 (riece-format-identity new-identity t)))
			  "\n"))
    (riece-redisplay-buffers)))

(defun riece-handle-privmsg-message (prefix string)
  (let* ((user (riece-prefix-nickname prefix))
	 (parameters (riece-split-parameters string))
	 (targets (split-string (car parameters) ","))
	 (message (nth 1 parameters)))
    (riece-display-message
     (riece-make-message (riece-make-identity user
					      riece-server-name)
			 (riece-make-identity (car targets)
					      riece-server-name)
			 message))))

(defun riece-handle-notice-message (prefix string)
  (let* ((user (if prefix
		   (riece-prefix-nickname prefix)))
	 (parameters (riece-split-parameters string))
	 (targets (split-string (car parameters) ","))
	 (message (nth 1 parameters)))
    (if user
	(riece-display-message
	 (riece-make-message (riece-make-identity user
						  riece-server-name)
			     (riece-make-identity (car targets)
						  riece-server-name)
			     message 'notice))
      ;; message from server
      (riece-insert-notice
       (list riece-dialogue-buffer riece-others-buffer)
       (concat (riece-concat-server-name message) "\n")))))

(defun riece-handle-ping-message (prefix string)
  (riece-send-string (format "PONG :%s\r\n"
			     (if (eq (aref string 0) ?:)
				 (substring string 1)
			       string))))

(defun riece-handle-join-message (prefix string)
  (let* ((user (riece-prefix-nickname prefix))
	 ;; RFC2812 3.2.1 doesn't recommend server to send join
	 ;; messages which contain multiple targets.
	 (channels (split-string (car (riece-split-parameters string)) ","))
	 (user-identity (riece-make-identity user riece-server-name)))
    (while channels
      (riece-naming-assert-join user (car channels))
      (let* ((channel-identity (riece-make-identity (car channels)
						    riece-server-name))
	     (buffer (get-buffer (riece-channel-buffer-name
				  channel-identity))))
	(riece-insert-change
	 buffer
	 (format "%s (%s) has joined %s\n"
		 (riece-format-identity user-identity t)
		 (riece-user-get-user-at-host user)
		 (riece-format-identity channel-identity t)))
	(riece-insert-change
	 (if (and riece-channel-buffer-mode
		  (not (eq buffer riece-channel-buffer)))
	     (list riece-dialogue-buffer riece-others-buffer)
	   riece-dialogue-buffer)
	 (concat
	  (riece-concat-server-name
	   (format "%s (%s) has joined %s"
		   (riece-format-identity user-identity t)
		   (riece-user-get-user-at-host user)
		   (riece-format-identity channel-identity t)))
	  "\n")))
      (setq channels (cdr channels)))
    (riece-redisplay-buffers)))

(defun riece-handle-part-message (prefix string)
  (let* ((user (riece-prefix-nickname prefix))
	 (parameters (riece-split-parameters string))
	 ;; RFC2812 3.2.2 doesn't recommend server to send part
	 ;; messages which contain multiple targets.
	 (channels (split-string (car parameters) ","))
	 (message (nth 1 parameters))
	 (user-identity (riece-make-identity user riece-server-name)))
    (while channels
      (riece-naming-assert-part user (car channels))
      (let* ((channel-identity (riece-make-identity (car channels)
						    riece-server-name))
	     (buffer (get-buffer (riece-channel-buffer-name
				  channel-identity))))
	(riece-insert-change
	 buffer
	 (concat
	  (riece-concat-message
	   (format "%s has left %s"
		   (riece-format-identity user-identity t)
		   (riece-format-identity channel-identity t))
	   message)
	  "\n"))
	(riece-insert-change
	 (if (and riece-channel-buffer-mode
		  (not (eq buffer riece-channel-buffer)))
	     (list riece-dialogue-buffer riece-others-buffer)
	   riece-dialogue-buffer)
	 (concat
	  (riece-concat-server-name
	   (riece-concat-message
	    (format "%s has left %s"
		    (riece-format-identity user-identity t)
		    (riece-format-identity channel-identity t))
	    message))
	  "\n")))
      (setq channels (cdr channels)))
    (riece-redisplay-buffers)))

(defun riece-handle-kick-message (prefix string)
  (let* ((kicker (riece-prefix-nickname prefix))
	 (parameters (riece-split-parameters string))
	 (channel (car parameters))
	 (user (nth 1 parameters))
	 (message (nth 2 parameters))
	 (kicker-identity (riece-make-identity kicker riece-server-name))
	 (channel-identity (riece-make-identity channel riece-server-name))
	 (user-identity (riece-make-identity user riece-server-name)))
    (riece-naming-assert-part user channel)
    (let ((buffer (get-buffer (riece-channel-buffer-name channel-identity))))
      (riece-insert-change
       buffer
       (concat
	(riece-concat-message
	 (format "%s kicked %s out from %s"
		 (riece-format-identity kicker-identity t)
		 (riece-format-identity user-identity t)
		 (riece-format-identity channel-identity t))
	 message)
	"\n"))
      (riece-insert-change
       (if (and riece-channel-buffer-mode
		(not (eq buffer riece-channel-buffer)))
	   (list riece-dialogue-buffer riece-others-buffer)
	 riece-dialogue-buffer)
       (concat
	(riece-concat-server-name
	 (riece-concat-message
	  (format "%s kicked %s out from %s\n"
		 (riece-format-identity kicker-identity t)
		 (riece-format-identity user-identity t)
		 (riece-format-identity channel-identity t))
	  message))
	"\n")))
    (riece-redisplay-buffers)))

(defun riece-handle-quit-message (prefix string)
  (let* ((user (riece-prefix-nickname prefix))
	 (channels (copy-sequence (riece-user-get-channels user)))
	 (pointer channels)
	 (parameters (riece-split-parameters string))
	 (message (car parameters))
	 (user-identity (riece-make-identity user riece-server-name)))
    ;; If you are talking with the user, quit it.
    (if (riece-identity-member user-identity riece-current-channels)
	(riece-part-channel user))
    (setq pointer channels)
    (while pointer
      (riece-naming-assert-part user (car pointer))
      (setq pointer (cdr pointer)))
    (let ((buffers
	   (mapcar
	    (lambda (channel)
	      (get-buffer
	       (riece-channel-buffer-name
		(riece-make-identity channel riece-server-name))))
	    channels)))
      (riece-insert-change
       buffers
       (concat
	(riece-concat-message
	 (format "%s has left IRC"
		 (riece-format-identity user-identity t))
	 message)
	"\n"))
      (riece-insert-change
       (if (and riece-channel-buffer-mode
		(not (memq riece-channel-buffer buffers)))
	   (list riece-dialogue-buffer riece-others-buffer)
	 riece-dialogue-buffer)
       (concat
	(riece-concat-server-name
	 (riece-concat-message
	  (format "%s has left IRC"
		  (riece-format-identity user-identity t))
	  message))
	"\n"))))
  (riece-redisplay-buffers))

(defun riece-handle-kill-message (prefix string)
  (let* ((killer (riece-prefix-nickname prefix))
	 (parameters (riece-split-parameters string))
	 (user (car parameters))
	 (message (nth 1 parameters))
	 (channels (copy-sequence (riece-user-get-channels user)))
	 (killer-identity (riece-make-identity killer riece-server-name))
	 (user-identity (riece-make-identity user riece-server-name))
	 pointer)
    ;; If you are talking with the user, quit it.
    (if (riece-identity-member user-identity riece-current-channels)
	(riece-part-channel user))
    (setq pointer channels)
    (while pointer
      (riece-naming-assert-part user (car pointer))
      (setq pointer (cdr pointer)))
    (let ((buffers
	   (mapcar
	    (lambda (channel)
	      (get-buffer
	       (riece-channel-buffer-name
		(riece-make-identity channel riece-server-name))))
	    channels)))
      (riece-insert-change
       buffers
       (concat
	(riece-concat-message
	 (format "%s killed %s"
		 (riece-format-identity killer-identity t)
		 (riece-format-identity user-identity t))
	 message)
	"\n"))
      (riece-insert-change
       (if (and riece-channel-buffer-mode
		(not (memq riece-channel-buffer buffers)))
	   (list riece-dialogue-buffer riece-others-buffer)
	 riece-dialogue-buffer)
       (concat
	(riece-concat-server-name
	 (riece-concat-message
	  (format "%s killed %s"
		 (riece-format-identity killer-identity t)
		 (riece-format-identity user-identity t))
	  message))
	"\n")))
    (riece-redisplay-buffers)))

(defun riece-handle-invite-message (prefix string)
  (let* ((user (riece-prefix-nickname prefix))
	 (parameters (riece-split-parameters string))
	 (invited (car parameters))
	 (channel (nth 1 parameters)))
    (riece-insert-info
     (list riece-dialogue-buffer riece-others-buffer)
     (concat
      (riece-concat-server-name
       (format "%s invites %s to %s"
	       (riece-format-identity (riece-make-identity
				       user riece-server-name))
	       (riece-format-identity (riece-make-identity
				       invited riece-server-name))
	       (riece-format-identity (riece-make-identity
				       channel riece-server-name))))
      "\n"))))

(defun riece-handle-topic-message (prefix string)
  (let* ((user (riece-prefix-nickname prefix))
	 (parameters (riece-split-parameters string))
	 (channel (car parameters))
	 (topic (nth 1 parameters))
	 (user-identity (riece-make-identity user riece-server-name))
	 (channel-identity (riece-make-identity channel riece-server-name)))
    (riece-channel-set-topic (riece-get-channel channel) topic)
    (let ((buffer (get-buffer (riece-channel-buffer-name channel-identity))))
      (riece-insert-change
       buffer
       (format "Topic by %s: %s\n"
	       (riece-format-identity user-identity t)
	       topic))
      (riece-insert-change
       (if (and riece-channel-buffer-mode
		(not (eq buffer riece-channel-buffer)))
	   (list riece-dialogue-buffer riece-others-buffer)
	 riece-dialogue-buffer)
       (concat
	(riece-concat-server-name
	 (format "Topic on %s by %s: %s"
		 (riece-format-identity channel-identity t)
		 (riece-format-identity user-identity t)
		 topic))
	"\n"))
      (riece-redisplay-buffers))))

(defsubst riece-parse-channel-modes (string channel)
  (while (string-match "^[-+]\\([^ ]*\\) *" string)
    (let ((toggle (aref string 0))
	  (modes (string-to-list (match-string 1 string))))
      (setq string (substring string (match-end 0)))
      (while modes
	(if (and (memq (car modes) '(?O ?o ?v ?k ?l ?b ?e ?I))
		 (string-match "\\([^-+][^ ]*\\) *" string))
	    (let ((parameter (match-string 1 string)))
	      (setq string (substring string (match-end 0)))
	      (cond
	       ((eq (car modes) ?o)
		(riece-channel-toggle-operator channel parameter
					       (eq toggle ?+)))
	       ((eq (car modes) ?v)
		(riece-channel-toggle-speaker channel parameter
					      (eq toggle ?+)))
	       ((eq (car modes) ?b)
		(riece-channel-toggle-banned channel parameter
					     (eq toggle ?+)))
	       ((eq (car modes) ?e)
		(riece-channel-toggle-uninvited channel parameter
						(eq toggle ?+)))
	       ((eq (car modes) ?I)
		(riece-channel-toggle-invited channel parameter
					      (eq toggle ?+)))))
	  (riece-channel-toggle-mode channel (car modes)
				     (eq toggle ?+)))
	(setq modes (cdr modes))))))

(defun riece-handle-mode-message (prefix string)
  (let* ((user (riece-prefix-nickname prefix))
	 (user-identity (riece-make-identity user riece-server-name))
	 channel)
    (when (string-match "\\([^ ]+\\) *:?" string)
      (setq channel (match-string 1 string)
	    string (substring string (match-end 0)))
      (riece-parse-channel-modes string channel)
      (let* ((channel-identity (riece-make-identity channel riece-server-name))
	     (buffer (get-buffer (riece-channel-buffer-name
				  channel-identity))))
	(riece-insert-change
	 buffer
	 (format "Mode by %s: %s\n"
		 (riece-format-identity user-identity t)
		 string))
	(riece-insert-change
	 (if (and riece-channel-buffer-mode
		  (not (eq buffer riece-channel-buffer)))
	     (list riece-dialogue-buffer riece-others-buffer)
	   riece-dialogue-buffer)
	 (concat
	  (riece-concat-server-name
	   (format "Mode on %s by %s: %s"
		   (riece-format-identity channel-identity t)
		   (riece-format-identity user-identity t)
		   string))
	  "\n"))
	(riece-redisplay-buffers)))))

(provide 'riece-handle)

;;; riece-handle.el ends here
