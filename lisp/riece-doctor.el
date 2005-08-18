;;; riece-doctor.el --- "become a psychotherapist" add-on
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
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This add-on allows you to become a psychotherapist.

;; To use, add the following line to your ~/.riece/init.el:
;; (add-to-list 'riece-addons 'riece-doctor)

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

(defvar riece-doctor-enabled nil)

(defconst riece-doctor-description
  "Allow users in channel to talk with the classic pseudo-AI")

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
  (if riece-doctor-enabled
      (let* ((user (riece-prefix-nickname prefix))
	     (parameters (riece-split-parameters string))
	     (targets (split-string (car parameters) ","))
	     (message (nth 1 parameters)))
	(if (string-match riece-doctor-hello-regexp message)
	    (if (riece-identity-member user riece-doctor-patients t)
		(riece-doctor-reply
		 (car targets)
		 (format "%s: You are already talking with me." user))
	      (save-excursion
		(set-buffer (get-buffer-create
			     (riece-doctor-buffer-name user)))
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
		  (save-excursion
		    (set-buffer (get-buffer (riece-doctor-buffer-name user)))
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

(defun riece-doctor-enable ()
  (setq riece-doctor-enabled t))

(defun riece-doctor-disable ()
  (setq riece-doctor-enabled nil))

(provide 'riece-doctor)

;;; riece-doctor.el ends here
