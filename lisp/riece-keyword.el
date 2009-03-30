;;; riece-keyword.el --- detect keywords in IRC buffers
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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; NOTE: This is an add-on module for Riece.

;;; Code:

(require 'riece-message)

(defgroup riece-keyword nil
  "Detect keywords in IRC buffers."
  :prefix "riece-"
  :group 'riece)

(defcustom riece-keywords nil
  "Keywords to be highlightened."
  :type '(repeat (choice (string :tag "Keyword")
			 (cons (string :tag "Regexp")
			       (integer :tag "Match"))))
  :group 'riece-keyword)

(defcustom riece-notify-keyword-functions nil
  "Functions used to notify keyword match."
  :type '(repeat function)
  :group 'riece-keyword)

(make-obsolete-variable 'riece-notify-keyword-functions
			'riece-keyword-notify-functions)

(defcustom riece-keyword-notify-functions nil
  "Functions used to notify keyword match.
Two arguments are passed to each function: the keyword used to match
and the matched message object."
  :type '(repeat function)
  :group 'riece-keyword)

(defface riece-keyword-face
  '((((class color))
     (:foreground "red" :underline t))
    (t
     (:underline t)))
  "Face used for highlightening matching keyword."
  :group 'riece-highlight-faces)
(defvar riece-keyword-face 'riece-keyword-face)

(defconst riece-keyword-description
  "Detect keywords in IRC buffers.")

;;; The old XEmacs package doesn't have autoload setting for regexp-opt.
(autoload 'regexp-opt "regexp-opt")
(defun riece-keyword-message-filter (message)
  (if (and (get 'riece-keyword 'riece-addon-enabled)
	   riece-keywords
	   ;; Ignore messages which belongs to myself.
	   (not (riece-message-own-p message)))
      (let* (keywords
	     (alist
	      (nconc
	       (delq nil (mapcar
			  (lambda (matcher)
			    (if (stringp matcher)
				(ignore
				 (setq keywords (cons matcher keywords)))
			      matcher))
			  riece-keywords))
	       (if keywords
		   (list (cons (regexp-opt keywords) 0)))))
	     index)
	(while alist
	  (setq index 0)
	  (while (and (< index (length (riece-message-text message)))
		      (string-match (car (car alist))
				    (riece-message-text message) index))
	    (put-text-property (match-beginning (cdr (car alist)))
			       (match-end (cdr (car alist)))
			       'riece-overlay-face riece-keyword-face
			       (riece-message-text message))
	    (save-match-data
	      (run-hook-with-args 'riece-notify-keyword-functions
				  (match-string (cdr (car alist))
						(riece-message-text message)))
	      (run-hook-with-args 'riece-keyword-notify-functions
				  (cdr (car alist))
				  message))
	    (setq index (1+ (match-end (cdr (car alist))))))
	  (setq alist (cdr alist)))))
  message)

(defun riece-keyword-requires ()
  (if (memq 'riece-highlight riece-addons)
      '(riece-highlight)))

(defun riece-keyword-insinuate ()
  (add-hook 'riece-message-filter-functions 'riece-keyword-message-filter))

(defun riece-keyword-uninstall ()
  (remove-hook 'riece-message-filter-functions 'riece-keyword-message-filter))

(provide 'riece-keyword)

;;; riece-keyword.el ends here