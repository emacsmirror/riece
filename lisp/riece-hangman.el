;;; riece-hangman.el --- hangman
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
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; To use, add the following line to your ~/.riece/init.el:
;; (add-to-list 'riece-addons 'riece-hangman t)

;;; Code:

(require 'riece-globals)
(require 'riece-identity)
(require 'riece-message)
(require 'riece-server)

(defgroup riece-hangman nil
  "Interface to hangman.el"
  :prefix "riece-"
  :group 'riece)

(defcustom riece-hangman-hello-regexp "^, hangman"
  "Pattern of string to start the game."
  :type 'string
  :group 'riece-hangman)

(defcustom riece-hangman-bye-regexp "^, bye hangman"
  "Pattern of string to end the game."
  :type 'string
  :group 'riece-hangman)

(defcustom riece-hangman-words-file "/usr/share/dict/words"
  "Location of words file."
  :type 'file
  :group 'riece-hangman)

(defvar riece-hangman-players nil)
(defvar riece-hangman-words-buffer nil)

(defun riece-hangman-make-context (word)
  (vector word nil 0))

(defun riece-hangman-context-word (context)
  (aref context 0))

(defun riece-hangman-context-guessed (context)
  (aref context 1))

(defun riece-hangman-context-missed-count (context)
  (aref context 2))

(defun riece-hangman-context-set-word (context word)
  (aset context 0 word))

(defun riece-hangman-context-set-guessed (context guessed)
  (aset context 1 guessed))

(defun riece-hangman-context-set-missed-count (context missed-count)
  (aset context 2 missed-count))

(defun riece-hangman-word ()
  (unless riece-hangman-words-buffer
    (setq riece-hangman-words-buffer (generate-new-buffer " *riece-hangman*"))
    (save-excursion
      (set-buffer riece-hangman-words-buffer)
      (buffer-disable-undo)
      (insert-file-contents riece-hangman-words-file)
      (let ((case-fold-search nil))
	(delete-non-matching-lines "^[a-z][a-z][a-z][a-z][a-z][a-z]"))))
  (save-excursion
    (set-buffer riece-hangman-words-buffer)
    (goto-char (% (1+ (random)) (buffer-size)))
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

(defun riece-hangman-reply-with-context (target context)
  (let* ((word (riece-hangman-context-word context))
	 (masked-word (make-string (length word) ?-))
	 (guessed (riece-hangman-context-guessed context))
	 (index 0))
    (while (< index (length word))
      (if (memq (aref word index) guessed)
	  (aset masked-word index (aref word index)))
      (setq index (1+ index)))
    (riece-hangman-reply
     target
     (format "Word: %s, Guessed: %s"
	     masked-word
	     (apply #'string (sort (copy-sequence guessed) #'<))))))

(defun riece-hangman-after-privmsg-hook (prefix string)
  (let* ((user (riece-make-identity (riece-prefix-nickname prefix)
				    riece-server-name))
	 (parameters (riece-split-parameters string))
	 (targets (split-string (car parameters) ","))
	 (message (nth 1 parameters))
	 pointer)
    (if (string-match riece-hangman-hello-regexp message)
	(if (riece-identity-assoc user riece-hangman-players)
	    (riece-hangman-reply
	     (car targets)
	     (format "%s: You are already playing the game."
		     (riece-format-identity user t)))
	  (let ((context (riece-hangman-make-context (riece-hangman-word))))
	    (setq riece-hangman-players (cons (cons user context)
					      riece-hangman-players))
	    (riece-hangman-reply-with-context (car targets) context)))
      (if (string-match riece-hangman-bye-regexp message)
	  (when (setq pointer (riece-identity-assoc user
						    riece-hangman-players))
	    (setq riece-hangman-players (delq pointer riece-hangman-players))
	    (riece-hangman-reply
	     (car targets)
	     (format "%s: Sorry, the word was \"%s\""
		     (riece-format-identity user t)
		     (riece-hangman-context-word (cdr pointer)))))
	(if (setq pointer (riece-identity-assoc user riece-hangman-players))
	    (if (or (/= (length message) 1)
		    (not (string-match "[a-z]" message)))
		(riece-hangman-reply
		 (car targets)
		 (format "%s: Not a valid guess: %s"
			 (riece-format-identity user t)
			 message))
	      (if (memq (aref message 0)
			(riece-hangman-context-guessed (cdr pointer)))
		  (riece-hangman-reply (car targets)
				       (format "%s: Already guessed '%c'"
					       (riece-format-identity user t)
					       (aref message 0)))
		(riece-hangman-context-set-guessed
		 (cdr pointer)
		 (cons (aref message 0)
		       (riece-hangman-context-guessed (cdr pointer))))
		(let ((word (riece-hangman-context-word (cdr pointer)))
		      (index 0)
		      (char (aref message 0)))
		  (unless (catch 'found
			    (while (< index (length word))
			      (if (eq (aref word index) char)
				  (throw 'found t))
			      (setq index (1+ index))))
		    (riece-hangman-context-set-missed-count
		     (cdr pointer)
		     (1+ (riece-hangman-context-missed-count
			  (cdr pointer))))))
		(if (>= (riece-hangman-context-missed-count (cdr pointer)) 7)
		    (progn
		      (riece-hangman-reply
		       (car targets)
		       (format "%s: Sorry, the word was \"%s\""
			       (riece-format-identity user t)
			       (riece-hangman-context-word (cdr pointer))))
		      (setq riece-hangman-players
			    (delq pointer
				  riece-hangman-players)))
		  (let ((word (riece-hangman-context-word (cdr pointer)))
			(guessed (riece-hangman-context-guessed (cdr pointer)))
			(index 0)
			(char (aref message 0)))
		    (if (catch 'missing
			  (while (< index (length word))
			    (unless (memq (aref word index) guessed)
			      (throw 'missing t))
			    (setq index (1+ index))))
			(riece-hangman-reply-with-context
			 (car targets) (cdr pointer))
		      (riece-hangman-reply
		       (car targets)
		       (format "%s: You got it!"
			       (riece-format-identity user t)))
		      (setq riece-hangman-players
			    (delq pointer riece-hangman-players))))))))))))

(defun riece-hangman-insinuate ()
  (add-hook 'riece-after-privmsg-hook 'riece-hangman-after-privmsg-hook))

(provide 'riece-hangman)

;;; riece-hangman.el ends here
