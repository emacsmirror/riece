;;; riece-url.el --- URL collector add-on
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
;; (add-to-list 'riece-addons 'riece-url)

;;; Code:

(require 'riece-options)
(require 'riece-menu)			;riece-menu-items

(defvar browse-url-browser-function)

(defgroup riece-url nil
  "URL Browsing in IRC buffer."
  :group 'riece-vars)

(defcustom riece-url-regexp  "\\b\\(s?https?\\|ftp\\|file\\|gopher\\|news\\|telnet\\|wais\\|mailto\\):\\(//[-a-zA-Z0-9_.]+:[0-9]*\\)?[-a-zA-Z0-9_=?#$@~`%&*+|\\/.,]*[-a-zA-Z0-9_=#$@~`%&*+|\\/]"
  "Regular expression that matches URLs."
  :group 'riece-url
  :type 'regexp)

(defvar riece-urls nil
  "A list of URL which appears in Riece buffers.")

(autoload 'widget-convert-button "wid-edit")

(defun riece-url-scan-region (start end)
  (save-excursion
    (goto-char start)
    (while (re-search-forward riece-url-regexp end t)
      (let ((url (match-string 0)))
	(if (memq 'riece-highlight riece-addons)
	    (widget-convert-button
	     'url-link (match-beginning 0) (match-end 0) url))
	(unless (member url riece-urls)
	  (setq riece-urls (cons url riece-urls)))))))

(defun riece-command-browse-url (&optional url)
  (interactive
   (list (completing-read "Open URL: " (mapcar #'list riece-urls))))
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

(defun riece-url-insinuate ()
  (add-hook 'riece-after-insert-functions 'riece-url-scan-region)
  (define-key riece-dialogue-mode-map "U" 'riece-command-browse-url)
  (if (memq 'riece-menu riece-addons)
      (add-hook 'riece-command-mode-hook
		(lambda ()
		  (easy-menu-add-item
		   nil (list (car riece-menu-items))
		   '("Open URL..." :filter riece-url-create-menu)))
		t)))

(provide 'riece-url)

;;; riece-url.el ends here
