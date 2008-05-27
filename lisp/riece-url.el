;;; riece-url.el --- collect URL in IRC buffers
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

(require 'riece-options)
(require 'riece-menu)			;riece-menu-items
(require 'easymenu)

(autoload 'browse-url "browse-url")
(defvar browse-url-browser-function)

(defgroup riece-url nil
  "URL Browsing in IRC buffer."
  :prefix "riece-"
  :group 'riece)

(defcustom riece-url-regexp  "\\b\\(s?https?\\|ftp\\|file\\|gopher\\|news\\|telnet\\|wais\\|mailto\\):\\(//[-a-zA-Z0-9_.]+:[0-9]*\\)?[-a-zA-Z0-9_=?#$@~`%&*+|\\/.,;]*[-a-zA-Z0-9_=#$@~`%&*+|\\/;]"
  "Regular expression that matches URLs."
  :group 'riece-url
  :type 'regexp)

(defcustom riece-url-regexp-alist nil
  "An alist mapping regexp to URL.
For example:
  (setq riece-url-regexp-alist
        '((\"\\\\bBug#\\\\([0-9]+\\\\)\\\\b\" .
           \"http://bugs.debian.org/\\\\1\")))

This maps a string \"Bug#12345\" to a URL
\"http://bugs.debian.org/12345\"."
  :type 'alist
  :group 'riece-url)

(defvar riece-urls nil
  "A list of URL which appears in Riece buffers.")

(defconst riece-url-description
  "Collect URL in IRC buffers.")

(autoload 'widget-convert-button "wid-edit")

(defun riece-url-replace-match (string)
  (let ((match-data (match-data))
	(index 0)
	number
	replacement)
    (while (string-match "\\\\[&1-9\\\\]" string index)
      (if (eq (aref string (1+ (match-beginning 0))) ?&)
	  (setq number 0)
	(unless (eq (aref string (1+ (match-beginning 0))) ?\\)
	  (setq number (string-to-number (substring (match-string 0 string)
						    1)))))
      (if number
	  (setq replacement
		(buffer-substring (nth (* number 2) match-data)
				  (nth (1+ (* number 2)) match-data)))
	(setq replacement "\\"))
      (setq string (concat (substring string 0 (match-beginning 0))
			   replacement
			   (substring string (match-end 0)))
	    index (+ index (length replacement))))
    string))

(defun riece-url-scan-region (start end)
  (let ((alist (cons (cons riece-url-regexp "\\&")
		     riece-url-regexp-alist)))
    (while alist
      (save-excursion
	(goto-char start)
	(while (re-search-forward (car (car alist)) end t)
	  (let ((url (save-match-data
		       (riece-url-replace-match (cdr (car alist))))))
	    (if (memq 'riece-highlight riece-addons)
		(widget-convert-button
		 'url-link (match-beginning 0) (match-end 0) url))
	    (unless (member url riece-urls)
	      (setq riece-urls (cons url riece-urls))))))
      (setq alist (cdr alist)))))

(defun riece-command-browse-url (&optional url)
  (interactive
   (list (completing-read (riece-mcat "Open URL: ")
			  (mapcar #'list riece-urls))))
  (browse-url url))

(defun riece-url-create-menu (menu)
  (mapcar (lambda (url)
	    (vector url (list 'browse-url url)))
	  riece-urls))

(defvar riece-dialogue-mode-map)

(defun riece-url-requires ()
  (append (if (memq 'riece-highlight riece-addons)
	      '(riece-highlight))
	  (if (memq 'riece-menu riece-addons)
	      '(riece-menu))))

(defun riece-url-command-mode-hook ()
  (easy-menu-add-item
   nil (list (car riece-menu-items))
   (list (if (featurep 'xemacs)
	     "Open URL..."
	   (riece-mcat "Open URL..."))
	 :filter 'riece-url-create-menu)))

(defun riece-url-insinuate ()
  (add-hook 'riece-after-insert-functions 'riece-url-scan-region)
  (if (memq 'riece-menu riece-addons)
      (add-hook 'riece-command-mode-hook
		'riece-url-command-mode-hook
		t)))

(defun riece-url-uninstall ()
  (easy-menu-remove-item
   nil (list (car riece-menu-items))
   (if (featurep 'xemacs)
       "Open URL..."
     (riece-mcat "Open URL...")))
  (remove-hook 'riece-after-insert-functions 'riece-url-scan-region)
  (remove-hook 'riece-command-mode-hook
	       'riece-url-command-mode-hook))

(defun riece-url-enable ()
  (define-key riece-dialogue-mode-map "U" 'riece-command-browse-url))

(defun riece-url-disable ()
  (define-key riece-dialogue-mode-map "U" nil))

(provide 'riece-url)

;;; riece-url.el ends here
