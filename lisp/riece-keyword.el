;;; riece-keyword.el --- highlight keywords in channel buffers
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

;;; Commentary:

;; To use, add the following line to your ~/.riece/init.el:
;; (add-to-list 'riece-addons 'riece-keyword)

;;; Code:

(require 'riece-message)

(defgroup riece-keyword nil
  "Highlight keyword in IRC buffer."
  :group 'riece-vars)

(defcustom riece-keywords nil
  "Keywords to be highlightened."
  :type '(repeat (choice (string :tag "Keyword")
			 (cons (string :tag "Regexp")
			       (integer :tag "Match"))))
  :group 'riece-keyword)

(defcustom riece-notify-keyword-functions nil
  "Functions used to notify keyword match."
  :type '(list function)
  :group 'riece-keyword)

(make-obsolete-variable 'riece-notify-keyword-functions
			'riece-keyword-notify-functions)

(defcustom riece-keyword-notify-functions nil
  "Functions used to notify keyword match.
Two arguments are passed to each function: the keyword used to match
and the matched message object."
  :type '(list function)
  :group 'riece-keyword)

(defface riece-keyword-face
  '((((class color))
     (:foreground "red" :underline t))
    (t
     ()))
  "Face used for highlightening matching keyword."
  :group 'riece-highlight-faces)
(defvar riece-keyword-face 'riece-keyword-face)

;;; The old XEmacs package doesn't have autoload setting for regexp-opt.
(autoload 'regexp-opt "regexp-opt")
(defun riece-keyword-message-filter (message)
  (if (and riece-keywords
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
	       (list (cons (regexp-opt keywords) 0))))
	     index)
	(while alist
	  (setq index 0)
	  (while (string-match (car (car alist))
			       (riece-message-text message) index)
	    (if (memq 'riece-highlight riece-addons)
		(put-text-property (match-beginning (cdr (car alist)))
				   (match-end (cdr (car alist)))
				   'riece-keyword t
				   (riece-message-text message)))
	    (run-hook-with-args 'riece-notify-keyword-functions
				(match-string (cdr (car alist))
					      (riece-message-text message)))
	    (run-hook-with-args 'riece-keyword-notify-functions
				(cdr (car alist))
				message)
	    (setq index (match-end (cdr (car alist)))))
	  (setq alist (cdr alist)))))
  message)

(defun riece-keyword-scan-region (start end)
  (riece-scan-property-region
   'riece-keyword
   start end
   (lambda (start end)
     (riece-overlay-put (riece-make-overlay start end)
			'face riece-keyword-face))))

(defun riece-keyword-requires ()
  (if (memq 'riece-highlight riece-addons)
      '(riece-highlight)))

(defun riece-keyword-insinuate ()
  (add-hook 'riece-message-filter-functions 'riece-keyword-message-filter)
  (add-hook 'riece-after-insert-functions 'riece-keyword-scan-region))

(provide 'riece-keyword)

;;; riece-keyword.el ends here