;;; riece-commands.el --- commands available in command buffer
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

(require 'riece-channel)
(require 'riece-complete)
(require 'riece-display)
(require 'riece-version)
(require 'riece-server)
(require 'riece-misc)
(require 'riece-identity)
(require 'riece-message)

;;; Channel movement:
(defun riece-command-switch-to-channel (channel)
  (interactive
   (list (completing-read "Channel/user: "
			  (mapcar #'list riece-current-channels)
			  nil t)))
  (riece-switch-to-channel channel)
  (riece-command-configure-windows))

(defun riece-command-switch-to-channel-by-number (number)
  (interactive
   (let ((command-name (symbol-name this-command)))
     (if (string-match "[0-9]+$" command-name)
	 (list (string-to-number (match-string 0 command-name)))
       (list (string-to-number (read-string "Number: "))))))
  (let ((channels riece-current-channels)
	(index 1))
    (while (and channels
		(< index number))
      (if (car channels)
	  (setq index (1+ index)))
      (setq channels (cdr channels)))
    (if (car channels)
	(riece-command-switch-to-channel (car channels))
      (error "No such number!"))))
	
(eval-and-compile
  (let ((number 1))
    (while (<= number 20)
      (defalias (intern (concat "riece-command-switch-to-channel-by-number-"
				(number-to-string number)))
	'riece-command-switch-to-channel-by-number)
      (setq number (1+ number)))))

(defun riece-command-next-channel ()
  "Select the next channel."
  (interactive)
  (let ((pointer (cdr (string-list-member-ignore-case
		       riece-current-channel
		       riece-current-channels))))
    (while (and pointer
		(null (car pointer)))
      (setq pointer (cdr pointer)))
    (if (car pointer)
	(riece-command-switch-to-channel (car pointer))
      (error "No such channel!"))))

(defun riece-command-previous-channel ()
  "Select the previous channel."
  (interactive)
  (let ((pointer (string-list-member-ignore-case
		  riece-current-channel
		  riece-current-channels))
	(start riece-current-channels)
	channel)
    (while (and start (not (eq start pointer)))
      (if (car start)
	  (setq channel (car start)))
      (setq start (cdr start)))
    (if channel
	(riece-command-switch-to-channel channel)
      (error "No such channel!"))))

(defun riece-command-select-command-buffer ()
  "Select the command buffer."
  (interactive)
  (let ((window (get-buffer-window riece-command-buffer)))
    (if window
	(select-window window))))

(defun riece-command-configure-windows ()
  (interactive)
  (riece-redisplay-buffers t))

(defun riece-command-toggle-channel-buffer-mode ()
  (interactive)
  (setq riece-channel-buffer-mode
	(not riece-channel-buffer-mode))
  (riece-command-configure-windows))

(defun riece-command-toggle-user-list-buffer-mode ()
  (interactive)
  (setq riece-user-list-buffer-mode
	(not riece-user-list-buffer-mode))
  (riece-command-configure-windows))

(defun riece-command-toggle-channel-list-buffer-mode ()
  (interactive)
  (setq riece-channel-list-buffer-mode
	(not riece-channel-list-buffer-mode))
  (riece-command-configure-windows))

(defun riece-get-users-on-server ()
  (riece-with-server-buffer
   (let (users)
     (mapatoms
      (lambda (atom)
	(unless (riece-channel-p (symbol-name atom))
	  (push (symbol-name atom) users)))
      riece-obarray)
     (if (member riece-real-nickname users)
	 users
       (cons riece-real-nickname users)))))

(defun riece-command-finger (user &optional recurse)
  (interactive
   (let* ((completion-ignore-case t)
	  (user (completing-read
		 "User: "
		 (mapcar #'list (riece-get-users-on-server)))))
     (list user current-prefix-arg)))
  (if recurse
      (riece-send-string (format "WHOIS %s %s\r\n" user user))
    (riece-send-string (format "WHOIS %s\r\n" user))))

(defun riece-command-topic (topic)
  (interactive
   (list (read-from-minibuffer
	  "Topic: " (cons (or (riece-channel-get-topic
			       riece-current-channel)
			      "")
			  0))))
  (riece-send-string (format "TOPIC %s :%s\r\n"
			     (riece-identity-prefix riece-current-channel)
			     topic)))

(defun riece-command-invite (&optional user channel)
  (interactive
   (let ((completion-ignore-case t)
	 user channel)
     (if current-prefix-arg
	 (setq channel
	       (completing-read
		"Channel: "
		(mapcar #'list riece-current-channels))))
     (list (completing-read
	    "User: "
	    (mapcar #'list (riece-get-users-on-server)))
	   channel)))
  (if channel
      (riece-send-string (format "INVITE %s %s\r\n"
				 user (riece-identity-prefix channel)))
    (riece-send-string (format "INVITE %s %s\r\n"
			       user (riece-identity-prefix
				     riece-current-channel)))))

(defun riece-command-change-mode (channel change)
  (interactive
   (let* ((completion-ignore-case t)
	  (channel
	   (if current-prefix-arg
	       (completing-read
		"Channel/user: "
		(mapcar #'list riece-current-channels))
	     riece-current-channel))
	  (riece-overriding-server-name (riece-identity-server channel))
	  (riece-temp-minibuffer-message
	   (concat "[Available modes: "
		   (riece-with-server-buffer
		    (if (and (riece-channel-p channel)
			     riece-supported-channel-modes)
			(apply #'string riece-supported-channel-modes)
		      (if (and (not (riece-channel-p channel))
			       riece-supported-user-modes)
			  (apply #'string riece-supported-user-modes))))
		   "]")))
     (list channel
	   (read-from-minibuffer
	    (concat (riece-concat-modes channel "Mode (? for help)") ": ")
	    nil riece-minibuffer-map))))
  (riece-send-string (format "MODE %s :%s\r\n" channel change)))

(defun riece-command-set-operators (users &optional arg)
  (interactive
   (let ((operators (riece-channel-get-operators riece-current-channel))
	 (completion-ignore-case t)
	 users)
     (if current-prefix-arg
	 (setq users (riece-completing-read-multiple
		      "Users"
		      (mapcar #'list operators)))
       (setq users (riece-completing-read-multiple
		    "Users"
		    (delq nil (mapcar (lambda (user)
					(unless (member user operators)
					  (list user)))
				      (riece-channel-get-users
				       riece-current-channel))))))
     (list users current-prefix-arg)))
  (let (group)
    (while users
      (push (pop users) group)
      (if (or (= (length group) 3)
	      (null users))
	  (riece-send-string
	   (format "MODE %s %c%s %s\r\n"
		   (riece-identity-prefix riece-current-channel)
		   (if current-prefix-arg
		       ?-
		     ?+)
		   (make-string (length group) ?o)
		   (mapconcat #'identity group " ")))))))

(defun riece-command-set-speakers (users &optional arg)
  (interactive
   (let ((speakers (riece-channel-get-speakers riece-current-channel))
	 (completion-ignore-case t)
	 users)
     (if current-prefix-arg
	 (setq users (riece-completing-read-multiple
		      "Users"
		      (mapcar #'list speakers)))
       (setq users (riece-completing-read-multiple
		    "Users"
		    (delq nil (mapcar (lambda (user)
					(unless (member user speakers)
					  (list user)))
				      (riece-channel-get-users
				       riece-current-channel))))))
     (list users current-prefix-arg)))
  (let (group)
    (while users
      (push (pop users) group)
      (if (or (= (length group) 3)
	      (null users))
	  (riece-send-string
	   (format "MODE %s %c%s %s\r\n"
		   (riece-identity-prefix riece-current-channel)
		   (if current-prefix-arg
		       ?-
		     ?+)
		   (make-string (length group) ?v)
		   (mapconcat #'identity group " ")))))))

(defun riece-command-send-message (message)
  "Send MESSAGE to the current channel."
  (if (equal message "")
      (error "No text to send"))
  (unless riece-current-channel
    (error (substitute-command-keys
	    "Type \\[riece-command-join] to join a channel")))
  (riece-send-string
   (format "PRIVMSG %s :%s\r\n"
	   (riece-identity-prefix riece-current-channel)
	   message))
  (riece-own-channel-message message))

(defun riece-command-enter-message ()
  "Send the current line to the current channel."
  (interactive)
  (riece-command-send-message (buffer-substring
			       (riece-line-beginning-position)
			       (riece-line-end-position)))
  (let ((next-line-add-newlines t))
    (next-line 1)))

(defun riece-command-join-channel (target key)
  (let ((server-name (riece-identity-server target))
	process)
    (if server-name
	(setq process (cdr (assoc server-name riece-server-process-alist)))
      (setq process riece-server-process))
    (unless process
      (error "%s" (substitute-command-keys
		   "Type \\[riece-command-open-server] to open server.")))
    (riece-process-send-string process
			       (if key
				   (format "JOIN %s :%s\r\n"
					   (riece-identity-prefix target)
					   key)
				 (format "JOIN %s\r\n"
					 (riece-identity-prefix target))))))

(defun riece-command-join-partner (target)
  (let ((pointer (riece-identity-member target riece-current-channels)))
    (if pointer
	(riece-command-switch-to-channel (car pointer))
      (riece-join-channel target)
      (riece-switch-to-channel target)
      (riece-redisplay-buffers))))

(defun riece-command-join (target &optional key)
  (interactive
   (let ((completion-ignore-case t)
	 (target
	  (completing-read "Channel/user: "
			   (mapcar #'list riece-current-channels)))
	 key)
     (if (and current-prefix-arg
	      (riece-channel-p target))
	 (setq key
	       (riece-read-passwd (format "Key for %s: " target))))
     (list target key)))
  (let ((pointer (riece-identity-member target riece-current-channels)))
    (if pointer
	(riece-command-switch-to-channel (car pointer))
      (if (riece-channel-p target)
	  (riece-command-join-channel target key)
	(riece-command-join-partner target)))))

(defun riece-command-part-channel (target message)
  (let ((server-name (riece-identity-server target))
	process)
    (if server-name
	(setq process (cdr (assoc server-name riece-server-process-alist)))
      (setq process riece-server-process))
    (unless process
      (error "%s" (substitute-command-keys
		   "Type \\[riece-command-open-server] to open server.")))
    (riece-process-send-string process
			       (if message
				   (format "PART %s :%s\r\n"
					   (riece-identity-prefix target)
					   message)
				 (format "PART %s\r\n"
					 (riece-identity-prefix target))))))

(defun riece-command-part (target &optional message)
  (interactive
   (let ((completion-ignore-case t)
	 (target
	  (completing-read "Channel/user: "
			   (mapcar #'list riece-current-channels)
			   nil t (cons riece-current-channel 0)))
	 message)
     (if (and current-prefix-arg
	      (riece-channel-p target))
	 (setq message (read-string "Message: ")))
     (list target message)))
  (if (riece-identity-member target riece-current-channels)
      (if (riece-channel-p target)
	  (riece-command-part-channel target message)
	(riece-part-channel target)
	(riece-redisplay-buffers))
    (error "You are not talking with %s" target)))

(defun riece-command-change-nickname (nickname)
  "Change your nickname to NICK."
  (interactive "sEnter your nickname: ")
  (riece-send-string (format "NICK %s\r\n" nickname)))

(defun riece-command-scroll-down (lines)
  "Scroll LINES down dialogue buffer from command buffer."
  (interactive "P")
  (let ((other-window-scroll-buffer
	 (if riece-channel-buffer-mode
	     riece-channel-buffer
	   riece-dialogue-buffer)))
    (when (get-buffer-window other-window-scroll-buffer)
      (condition-case nil
	  (scroll-other-window-down lines)
	(beginning-of-buffer
	 (message "Beginning of buffer"))))))

(defun riece-command-scroll-up (lines)
  "Scroll LINES up dialogue buffer from command buffer."
  (interactive "P")
  (let* ((other-window-scroll-buffer
	  (if riece-channel-buffer-mode
	      riece-channel-buffer
	    riece-dialogue-buffer)))
    (when (get-buffer-window other-window-scroll-buffer)
      (condition-case nil
	  (scroll-other-window lines)
	(end-of-buffer
	 (message "End of buffer"))))))

(defun riece-command-nick-scroll-down (lines)
  "Scroll LINES down nick buffer from command buffer."
  (interactive "P")
  (let ((other-window-scroll-buffer riece-user-list-buffer))
    (when (get-buffer-window other-window-scroll-buffer)
      (condition-case nil
	  (scroll-other-window-down lines)
	(beginning-of-buffer
	 (message "Beginning of buffer"))))))

(defun riece-command-nick-scroll-up (lines)
  "Scroll LINES up nick buffer from command buffer."
  (interactive "P")
  (let* ((other-window-scroll-buffer riece-user-list-buffer))
    (when (get-buffer-window other-window-scroll-buffer)
      (condition-case nil
	  (scroll-other-window lines)
	(end-of-buffer
	 (message "End of buffer"))))))

(defun riece-command-toggle-away (&optional message)
  "Mark yourself as being away."
  (interactive
   (if current-prefix-arg
       (let ((message (read-string "Away message: ")))
	 (list message))))
  (if message
      (riece-send-string (format "AWAY :%s\r\n" message))
    (riece-send-string "AWAY\r\n")))

(defun riece-command-toggle-freeze (&optional arg)
  "Prevent automatic scrolling of the dialogue window.
If prefix argument ARG is non-nil, toggle frozen status."
  (interactive "P")
  (riece-freeze (if riece-channel-buffer-mode
		    riece-channel-buffer
		  riece-dialogue-buffer)
		(if arg (prefix-numeric-value arg))))

(defun riece-command-toggle-own-freeze (&optional arg)
  "Prevent automatic scrolling of the dialogue window.
The difference from `riece-command-freeze' is that your messages are hidden.
If prefix argument ARG is non-nil, toggle frozen status."
  (interactive "P")
  (riece-own-freeze (if riece-channel-buffer-mode
			riece-channel-buffer
		      riece-dialogue-buffer)
		    (if arg (prefix-numeric-value arg))))

(defun riece-command-quit (&optional arg)
  "Quit IRC."
  (interactive "P")
  (if (y-or-n-p "Really quit IRC? ")
      (let ((message
	     (if arg
		 (read-string "Message: ")
	       (or riece-quit-message
		   (riece-extended-version)))))
	(riece-close-all-server message))))

(defun riece-command-raw (command)
  "Enter raw IRC command, which is sent to the server."
  (interactive "sIRC command: ")
  (riece-send-string (concat command "\r\n")))

(defun riece-command-end-of-buffer ()
  "Get end of the dialogue buffer."
  (interactive)
  (let (buffer window)
    (setq buffer (if riece-channel-buffer-mode
		     riece-channel-buffer
		   riece-dialogue-buffer))
    (or (setq window (get-buffer-window buffer))
	(setq window (get-buffer-window riece-dialogue-buffer)
	      buffer riece-dialogue-buffer))
    (when window
      (save-selected-window
	(select-window window)
	(goto-char (point-max))))))

(defun riece-command-copy-region (start end)
  "Move current region between START and END to `kill-ring'."
  (interactive "r")
  (kill-new (buffer-substring-no-properties start end)))

(defun riece-command-open-server (server-name)
  (interactive
   (list (completing-read "Server: " riece-server-alist)))
  (let ((process (riece-start-server
		  (riece-server-name-to-server server-name)
		  server-name)))
    (with-current-buffer (process-buffer process)
      (setq riece-server-name server-name))
    (push (cons server-name process) riece-server-process-alist)))

(defun riece-command-close-server (server-name &optional message)
  (interactive
   (list (completing-read "Server: " riece-server-process-alist)
	 (if current-prefix-arg
	     (read-string "Message: ")
	   (or riece-quit-message
	       (riece-extended-version)))))
  (riece-close-server server-name message))

(defun riece-command-universal-server-name-argument ()
  (interactive)
  (let* ((riece-overriding-server-name
	  (completing-read "Server: "
			   riece-server-process-alist))
	 (command
	  (key-binding (read-key-sequence
			(format "Command to execute on \"%s\":"
				riece-overriding-server-name)))))
    (message "")
    (call-interactively command)))

(provide 'riece-commands)

;;; riece-commands.el ends here
