;;; riece-desktop-notify.el --- Display notification to desktop
;; Copyright (C) 2009 OHASHI Akira

;; Author: OHASHI Akira <bg66@koka-in.org>
;; Created: 2009-03-29
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

;; riece-desktop-notify.el needs an external program to notify desktop.
;; The setting prepared beforehand for most popular OS uses the
;; following external programs.
;;
;; Mac OS X:
;;   growlnotify <URL:http://growl.info/extras.php#growlnotify>
;;   (of course you need Growl <URL:http://growl.info>)
;;
;; GNU/Linux:
;;   notify-send (which is included in libnotify)
;;    <URL:http://www.galago-project.org/news/index.php>
;;
;;  If you use Debian, you can install it by the following command:
;;
;;    % sudo aptitude install libnotify-bin
;;
;; Windows:
;;   Snarl_CMD.exe <URL:http://tlhan-ghun.de/?q=node/59>
;;   (of course you need Snarl <URL:http://www.fullphat.net/>)

;;; Code:

(require 'riece-message)
(eval-when-compile (require 'riece-keyword))

(defconst riece-desktop-notify-description
  "Display notification to desktop.")

(defgroup riece-desktop-notify nil
  "Display notification to desktop."
  :group 'riece)

(defcustom riece-desktop-notify-title-function
  '(lambda (message)
     (riece-identity-prefix (riece-message-target message)))
  "*The function which make title.
This function must have only one message object as argument."
  :type 'function
  :group 'riece-desktop-notify)

(defcustom riece-desktop-notify-message-function 'riece-format-message
  "*The function which make message.
This function must have only one message object as argument."
  :type 'function
  :group 'riece-desktop-notify)

(defcustom riece-desktop-notify-coding-system (terminal-coding-system)
  "*Coding system used to notify desktop."
  :type 'coding-system
  :group 'riece-desktop-notify)

(defcustom riece-desktop-notify-type
  (if (eq system-type 'linux) 'gnu/linux system-type)
  "*The type to notify desktop."
  :type '(radio (const :tag "Like Darwin" darwin)
		(const :tag "Like GNU/Linux" gnu/linux)
		(const :tag "Like Windows" windows-nt)
		(symbol :tag "The other type"))
  :group 'riece-desktop-notify)

;; for Darwin
(defcustom riece-desktop-notify-darwin-program "growlnotify"
  "*The program name to notify for darwin."
  :type 'file
  :group 'riece-desktop-notify)

(defcustom riece-desktop-notify-darwin-args
  '("-t" title "-m" message "-H" "localhost")
  "*The Arguments to notify for darwin."
  :type '(repeat (radio (string :tag "Argument")
			(const :tag "Title" title)
			(const :tag "Message" message)))
  :group 'riece-desktop-notify)

;; for GNU/Linux
(defcustom riece-desktop-notify-gnu/linux-program "notify-send"
  "*The program name to notify for GNU/Linux."
  :type 'file
  :group 'riece-desktop-notify)

(defcustom riece-desktop-notify-gnu/linux-args '("-u" "low" title message)
  "*The Arguments to notify for GNU/Linux."
  :type '(repeat (radio (string :tag "Argument")
			(const :tag "Title" title)
			(const :tag "Message" message)))
  :group 'riece-desktop-notify)

;; for Windows
(defcustom riece-desktop-notify-windows-nt-program "snarl_cmd.exe"
  "*The program name to notify for Windows."
  :type 'file
  :group 'riece-desktop-notify)

(defcustom riece-desktop-notify-windows-nt-args
  '("snShowMessage" "-1" title message)
  "*The Arguments string to notify for Windows."
  :type '(repeat (radio (string :tag "Argument")
			(const :tag "Title" title)
			(const :tag "Message" message)))
  :group 'riece-desktop-notify)

;; stolen and modified from riece-ruby.el
(defun riece-desktop-notify-substitute-variables (args alist)
  "Substitute symbols in ARGS by looking up ALIST."
  (setq args (copy-sequence args))
  (while alist
    (let ((pointer args))
      (while pointer
	(setq pointer (memq (car (car alist)) args))
	(if pointer
	    (setcar pointer (cdr (car alist))))))
    (setq alist (cdr alist)))
  args)

(defsubst riece-desktop-notify-make-symbol (symbol)
  (intern (format "riece-desktop-notify-%s-%s"
		  (symbol-name riece-desktop-notify-type)
		  (symbol-name symbol))))

(defun riece-desktop-notify-keyword-notify-function (keyword message)
  (let ((program-symbol (riece-desktop-notify-make-symbol 'program))
	(args-symbol (riece-desktop-notify-make-symbol 'args)))
    (when (and (boundp program-symbol) (boundp args-symbol))
      (let ((program (eval program-symbol))
	    (args (eval args-symbol)))
	(when (fboundp 'executable-find)
	  (setq program (executable-find program)))
	(when (stringp program)
	  (let ((title (funcall riece-desktop-notify-title-function message))
		(message (funcall riece-desktop-notify-message-function
				  message)))
	    (condition-case nil
		(apply #'call-process program nil nil nil
		       (riece-desktop-notify-substitute-variables
			args
			(list (cons 'title
				    (encode-coding-string
				     title
				     riece-desktop-notify-coding-system))
			      (cons 'message
				    (encode-coding-string
				     message
				     riece-desktop-notify-coding-system)))))
	      (file-error nil))))))))

(defun riece-desktop-notify-requires ()
  '(riece-keyword))

(defun riece-desktop-notify-insinuate ()
  (add-hook 'riece-keyword-notify-functions
	    'riece-desktop-notify-keyword-notify-function))

(defun riece-desktop-notify-uninstall ()
  (remove-hook 'riece-keyword-notify-functions
	       'riece-desktop-notify-keyword-notify-function))

(provide 'riece-desktop-notify)

;;; riece-desktop-notify.el ends here
