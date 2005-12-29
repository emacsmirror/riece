;;; riece-eval-ruby.el --- evaluate input string as a Ruby program
;; Copyright (C) 1998-2005 Daiki Ueno

;; Author: Daiki Ueno <ueno@unixuser.org>
;; Created: 1998-09-28
;; Keywords: IRC, riece, Ruby

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

(require 'riece-ruby)
(require 'riece-message)

(defgroup riece-eval-ruby nil
  "Evaluate input string as a Ruby program."
  :prefix "riece-"
  :group 'riece)

(defcustom riece-eval-ruby-prefix-regexp "^,ruby\\s-+"
  "Pattern of of the prefix for sending Ruby programs."
  :type 'string
  :group 'riece-eval-ruby)

(defconst riece-eval-ruby-description
  "Evaluate input string as a Ruby program.")

(defun riece-eval-ruby-exit-handler (name)
  (riece-ruby-inspect name)
  (let* ((data (copy-sequence (or riece-ruby-data "nil")))
	 (length (length data))
	 (index 0))
    (while (< index length)
      (if (eq (aref data index) ?\n)
	  (aset data index ? ))
      (setq index (1+ index)))
    (riece-send-string
     (format "NOTICE %s :%s\r\n"
	     (riece-identity-prefix
	      (riece-ruby-property name 'riece-eval-ruby-target))
	     data))
    (riece-display-message
     (riece-make-message (riece-current-nickname)
			 (riece-ruby-property name 'riece-eval-ruby-target)
			 data
			 'notice))))

(defun riece-eval-ruby-display-message-function (message)
  (if (and (get 'riece-eval-ruby 'riece-addon-enabled)
	   (riece-message-own-p message)
	   (string-match riece-eval-ruby-prefix-regexp
			 (riece-message-text message)))
      (let ((name (riece-ruby-execute
		   (substring (riece-message-text message)
			      (match-end 0)))))
	(riece-ruby-set-property name
				 'riece-eval-ruby-target
				 (riece-message-target message))
	(riece-ruby-set-exit-handler name
				     #'riece-eval-ruby-exit-handler))))

(defun riece-eval-ruby-insinuate ()
  (add-hook 'riece-after-display-message-functions
	    'riece-eval-ruby-display-message-function))

(defun riece-eval-ruby-uninstall ()
  (remove-hook 'riece-after-display-message-functions
	       'riece-eval-ruby-display-message-function))

(provide 'riece-eval-ruby)

;;; riece-eval-ruby.el ends here
