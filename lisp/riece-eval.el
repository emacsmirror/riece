;;; riece-eval.el --- eval add-on
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
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This add-on evaluates an input string as lisp object and sends a result
;; as notice. Note the risky of this add-on.

;; To use, add the following line to your ~/.riece/init.el:
;; (add-to-list 'riece-addons 'riece-eval)

;;; Code:

(require 'pp)
(require 'riece-message)

(defgroup riece-eval nil
  "Evaluate an input string as lisp object."
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

(defvar riece-eval-enabled nil)

(defconst riece-eval-description
  "Evaluate an input string as lisp object.")

(defun riece-eval-display-message-function (message)
  (when (and riece-eval-enabled
	     (riece-message-own-p message)
	     (string-match riece-eval-regexp (riece-message-text message)))
    (let ((form (match-string 1 (riece-message-text message)))
	  object string)
      (condition-case err
	  (progn
	    (setq object (eval (read form)))
	    (setq string
		  (cond
		   ((stringp object) object)
		   ((and (listp object)
			 (not (eq object nil)))
		    (let ((string (pp-to-string object)))
		      (substring string 0 (1- (length string)))))
		   ((numberp object)
		    (number-to-string object))
		   ((eq object nil) "")
		   (t (pp-to-string object)))))
	(error
	 (unless riece-eval-ignore-error
	     (setq string (format "Error evaluating %s: %s" form err)))))
      (unless (equal string "")
	(riece-send-string
	 (format "NOTICE %s :%s\r\n"
		 (riece-identity-prefix (riece-message-target message))
		 string))
	(riece-display-message
	 (riece-make-message (riece-current-nickname)
			     (riece-message-target message)
			     string 'notice))))))

(defun riece-eval-insinuate ()
  (add-hook 'riece-after-display-message-functions
	    'riece-eval-display-message-function))

(defun riece-eval-enable ()
  (setq riece-eval-enabled t))

(defun riece-eval-disable ()
  (setq riece-eval-enabled nil))

(provide 'riece-eval)

;;; riece-eval.el ends here
