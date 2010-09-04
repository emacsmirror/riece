;;; riece-doctor.el --- pretend to be a psychotherapist
;; Copyright (C) 1998-2003 Daiki Ueno

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

(defgroup riece-doctor nil
  "Interface to doctor.el."
  :prefix "riece-"
  :group 'riece)

(defcustom riece-doctor-hello-regexp "^,doctor$"
  "Pattern of string patients start consultation."
  :type 'string
  :group 'riece-doctor)

(defcustom riece-doctor-bye-regexp "^,doctor bye$"
  "Pattern of string patients end consultation."
  :type 'string
  :group 'riece-doctor)

(defvar riece-doctor-patients nil)

(defconst riece-doctor-description
  "Pretend to be a psychotherapist.")

(put 'riece-doctor 'riece-addon-default-disabled t)

(autoload 'doctor-mode "doctor")
(autoload 'doctor-read-print "doctor")

(defun riece-doctor-buffer-name (user)
  (concat " *riece-doctor*"
	  (riece-format-identity
	   (riece-make-identity user riece-server-name))))

(defun riece-doctor-reply (target string)
  (riece-display-message
   (riece-make-message (riece-make-identity riece-real-nickname
					    riece-server-name)
		       (riece-make-identity target riece-server-name)
		       string 'notice t))
  (riece-send-string (format "NOTICE %s :%s\r\n" target string)))

(defun riece-doctor-after-privmsg-hook (prefix string)
  (if (get 'riece-doctor 'riece-addon-enabled)
      (let* ((user (riece-prefix-nickname prefix))
	     (parameters (riece-split-parameters string))
	     (targets (split-string (car parameters) ","))
	     (message (nth 1 parameters)))
	(if (string-match riece-doctor-hello-regexp message)
	    (if (riece-identity-member user riece-doctor-patients t)
		(riece-doctor-reply
		 (car targets)
		 (format "%s: You are already talking with me." user))
	      (with-current-buffer (get-buffer-create
				    (riece-doctor-buffer-name user))
		(erase-buffer)
		(doctor-mode))
	      (setq riece-doctor-patients (cons user riece-doctor-patients))
	      (riece-doctor-reply
	       (car targets)
	       (format
		"%s: I am the psychotherapist.  \
Please, describe your problems."
		user)))
	  (if (string-match riece-doctor-bye-regexp message)
	      (let ((pointer (riece-identity-member user
						    riece-doctor-patients t)))
		(when pointer
		  (kill-buffer (riece-doctor-buffer-name user))
		  (setq riece-doctor-patients (delq (car pointer)
						    riece-doctor-patients))
		  (riece-doctor-reply
		   (car targets)
		   (format "%s: Good bye." user))))
	    (if (riece-identity-member user riece-doctor-patients t)
		(let (string)
		  (with-current-buffer (riece-doctor-buffer-name user)
		    (goto-char (point-max))
		    (insert message "\n")
		    (let ((point (point)))
		      (doctor-read-print)
		      (setq string (buffer-substring (1+ point)
						     (- (point) 2))))
		    (with-temp-buffer
		      (insert string)
		      (subst-char-in-region (point-min) (point-max) ?\n ? )
		      (setq string (buffer-string))))
		  (riece-doctor-reply
		   (car targets)
		   (format "%s: %s" user string)))))))))

(defun riece-doctor-insinuate ()
  (add-hook 'riece-after-privmsg-hook 'riece-doctor-after-privmsg-hook))

(defun riece-doctor-uninstall ()
  (remove-hook 'riece-after-privmsg-hook 'riece-doctor-after-privmsg-hook))

(provide 'riece-doctor)

;;; riece-doctor.el ends here
