;;; riece-eval.el --- evaluate input string as an elisp form
;; Copyright (C) 2005 OHASHI Akira

;; Author: OHASHI Akira <bg66@koka-in.org>
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

(require 'pp)
(require 'riece-message)

(defgroup riece-eval nil
  "Evaluate an input string as an elisp form."
  :prefix "riece-"
  :group 'riece)

(defcustom riece-eval-regexp "^, \\(.+\\)"
  "*Pattern of string evaluating."
  :type 'string
  :group 'riece-eval)

(defcustom riece-eval-ignore-error nil
  "*If non-nil, an error is ignored."
  :type 'boolean
  :group 'riece-eval)

(defconst riece-eval-description
  "Evaluate an input string as an elisp form.")

(defun riece-eval-display-message-function (message)
  (when (and (get 'riece-eval 'riece-addon-enabled)
	     (riece-message-own-p message)
	     (string-match riece-eval-regexp (riece-message-text message)))
    (let* ((form (match-string 1 (riece-message-text message)))
	   (string (riece-eval-form form)))
      (unless (equal string "")
	(riece-send-string
	 (format "NOTICE %s :%s\r\n"
		 (riece-identity-prefix (riece-message-target message))
		 string))
	(riece-display-message
	 (riece-make-message (riece-current-nickname)
			     (riece-message-target message)
			     string 'notice))))))

(defun riece-eval-form (form)
  (condition-case err
      (let ((object (eval (read form))))
	(cond
	 ((stringp object) object)
	 ((and (listp object)
	       (not (eq object nil)))
	  (let ((string (pp-to-string object)))
	    (substring string 0 (1- (length string)))))
	 ((numberp object)
	  (number-to-string object))
	 ((eq object nil) "")
	 (t (pp-to-string object))))
    (error
     (unless riece-eval-ignore-error
       (format "Error evaluating %s: %s" form err)))))

(defun riece-eval-insinuate ()
  (add-hook 'riece-after-display-message-functions
	    'riece-eval-display-message-function))

(defun riece-eval-uninstall ()
  (remove-hook 'riece-after-display-message-functions
	       'riece-eval-display-message-function))

(provide 'riece-eval)

;;; riece-eval.el ends here
