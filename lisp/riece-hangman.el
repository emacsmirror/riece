;;; riece-hangman.el --- allow channel members to play the hangman game
;; Copyright (C) 1998-2004 Daiki Ueno

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

(require 'riece-globals)
(require 'riece-identity)
(require 'riece-message)
(require 'riece-server)

(defgroup riece-hangman nil
  "Allow channel members to play the hangman game."
  :prefix "riece-"
  :group 'riece)

(defcustom riece-hangman-hello-regexp "^,hangman$"
  "Pattern of string to start the game."
  :type 'string
  :group 'riece-hangman)

(defcustom riece-hangman-bye-regexp "^,hangman bye$"
  "Pattern of string to end the game."
  :type 'string
  :group 'riece-hangman)

(defcustom riece-hangman-words-file "/usr/share/dict/words"
  "Location of words file."
  :type 'file
  :group 'riece-hangman)

(defvar riece-hangman-player-context-alist nil)
(defvar riece-hangman-words-buffer nil)

(defconst riece-hangman-description
  "Allow channel members to play the hangman game.")

(put 'riece-hangman 'riece-addon-default-disabled t)

(defun riece-hangman-make-context (word)
  "Make an instance of player context object.
This function is for internal use only."
  (vector word nil 0))

(defun riece-hangman-context-word (context)
  "Return the correct word of CONTEXT.
This function is for internal use only."
  (aref context 0))

(defun riece-hangman-context-guessed (context)
  "Return the guessed letters in this CONTEXT.
This function is for internal use only."
  (aref context 1))

(defun riece-hangman-context-missed-count (context)
  "Return the count of missed guesses in this CONTEXT.
This function is for internal use only."
  (aref context 2))

(defun riece-hangman-context-set-guessed (context guessed)
  "Set the GUESSED letters in this CONTEXT.
This function is for internal use only."
  (aset context 1 guessed))

(defun riece-hangman-context-set-missed-count (context missed-count)
  "Set the count of MISSED guesses in this CONTEXT.
This function is for internal use only."
  (aset context 2 missed-count))

(defun riece-hangman-word ()
  "Return random word.
The wordlist is read from `riece-hangman-words-file'."
  (unless (and riece-hangman-words-buffer
	       (buffer-name riece-hangman-words-buffer))
    (setq riece-hangman-words-buffer (generate-new-buffer " *riece-hangman*"))
    (with-current-buffer riece-hangman-words-buffer
      (buffer-disable-undo)
      (insert-file-contents riece-hangman-words-file)
      (let ((case-fold-search nil))
	(delete-non-matching-lines "^[a-z][a-z][a-z][a-z][a-z][a-z]+"))))
  (with-current-buffer riece-hangman-words-buffer
    (goto-char (1+ (random (buffer-size))))
    (if (eobp)
	(beginning-of-line -1)
      (beginning-of-line))
    (buffer-substring (point) (progn (end-of-line) (point)))))

(defun riece-hangman-reply (target string)
  (riece-display-message
   (riece-make-message (riece-make-identity riece-real-nickname
					    riece-server-name)
		       (riece-make-identity target riece-server-name)
		       string 'notice t))
  (riece-send-string (format "NOTICE %s :%s\r\n" target string)))

(defun riece-hangman-reply-with-context (user target context)
  (let ((masked-word (make-string
		      (length (riece-hangman-context-word context))
		      ?-))
	(guessed (copy-sequence (riece-hangman-context-guessed context)))
	(index 0))
    (while (< index (length (riece-hangman-context-word context)))
      (if (memq (aref (riece-hangman-context-word context) index) guessed)
	  (aset masked-word index
		(aref (riece-hangman-context-word context) index)))
      (setq index (1+ index)))
    (riece-hangman-reply
     target
     (format "%s: Word: %s, Guessed: %s"
	     user masked-word
	     (if guessed
		 (apply #'string (sort guessed #'<))
	       "")))))

(defun riece-hangman-after-privmsg-hook (prefix string)
  (if (get 'riece-hangman 'riece-addon-enabled)
      (let* ((user (riece-prefix-nickname prefix))
	     (parameters (riece-split-parameters string))
	     (targets (split-string (car parameters) ","))
	     (message (nth 1 parameters))
	     case-fold-search
	     pointer word guessed index)
	(if (string-match riece-hangman-hello-regexp message)
	    (if (riece-identity-assoc user riece-hangman-player-context-alist
				      t)
		(riece-hangman-reply
		 (car targets)
		 (format "%s: You are already playing the game." user))
	      (let ((context (riece-hangman-make-context
			      (riece-hangman-word))))
		(setq riece-hangman-player-context-alist
		      (cons (cons user context)
			    riece-hangman-player-context-alist))
		(riece-hangman-reply-with-context user (car targets) context)))
	  (if (string-match riece-hangman-bye-regexp message)
	      (when (setq pointer (riece-identity-assoc
				   user riece-hangman-player-context-alist t))
		(setq riece-hangman-player-context-alist
		      (delq pointer riece-hangman-player-context-alist))
		(riece-hangman-reply
		 (car targets)
		 (format "%s: Sorry, the word was \"%s\""
			 user
			 (riece-hangman-context-word (cdr pointer)))))
	    (if (setq pointer (riece-identity-assoc
			       user riece-hangman-player-context-alist t))
		(if (or (/= (length message) 1)
			(not (string-match "[a-z]" message)))
		    (riece-hangman-reply
		     (car targets)
		     (format "%s: Not a valid guess: %s" user message))
		  (if (memq (aref message 0)
			    (riece-hangman-context-guessed (cdr pointer)))
		      (riece-hangman-reply (car targets)
					   (format "%s: Already guessed '%c'"
						   user (aref message 0)))
		    (setq guessed (riece-hangman-context-set-guessed
				   (cdr pointer)
				   (cons (aref message 0)
					 (riece-hangman-context-guessed
					  (cdr pointer))))
			  word (riece-hangman-context-word (cdr pointer)))
		    (unless (catch 'found
			      (setq index 0)
			      (while (< index (length word))
				(if (eq (aref word index) (aref message 0))
				    (throw 'found t))
				(setq index (1+ index))))
		      (riece-hangman-context-set-missed-count
		       (cdr pointer)
		       (1+ (riece-hangman-context-missed-count
			    (cdr pointer)))))
		    (if (>= (riece-hangman-context-missed-count (cdr pointer))
			    7)
			(progn
			  (riece-hangman-reply
			   (car targets)
			   (format "%s: Sorry, the word was \"%s\""
				   user
				   (riece-hangman-context-word (cdr pointer))))
			  (setq riece-hangman-player-context-alist
				(delq pointer
				      riece-hangman-player-context-alist)))
		      (if (catch 'missing
			    (setq index 0)
			    (while (< index (length word))
			      (unless (memq (aref word index) guessed)
				(throw 'missing t))
			      (setq index (1+ index))))
			  (riece-hangman-reply-with-context user (car targets)
							    (cdr pointer))
			(riece-hangman-reply (car targets)
					     (format "%s: You got it! (%s)"
						     user word))
			(setq riece-hangman-player-context-alist
			      (delq
			       pointer
			       riece-hangman-player-context-alist))))))))))))

(defun riece-hangman-insinuate ()
  (add-hook 'riece-after-privmsg-hook 'riece-hangman-after-privmsg-hook))

(defun riece-hangman-uninstall ()
  (remove-hook 'riece-after-privmsg-hook 'riece-hangman-after-privmsg-hook))

(defun riece-hangman-enable ()
  (random t))

(provide 'riece-hangman)

;;; riece-hangman.el ends here
