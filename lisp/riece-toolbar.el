;;; riece-toolbar.el --- show toolbar icons
;; Copyright (C) 1998-2004 Daiki Ueno

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
;; (add-to-list 'riece-addons 'riece-toolbar)

;;; Code:

(require 'riece-menu)

(defconst riece-toolbar-description
  "Show toolbar icons.")

(defvar riece-toolbar-items
  '(riece-command-quit
    riece-command-join
    riece-command-part
    riece-command-previous-channel
    riece-command-next-channel
    riece-command-change-layout
    riece-submit-bug-report))

(defun riece-toolbar-find-menu-item (command)
  (let ((pointer riece-menu-items)
	item)
    (while pointer
      (if (and (not (stringp (car pointer)))
	       (vectorp (car pointer))
	       (eq (aref (car pointer) 1) command))
	  (setq item (car pointer)
		pointer nil)
	(setq pointer (cdr pointer))))
    item))

(if (featurep 'xemacs)
    (if (featurep 'toolbar)
	(progn
	  (defun riece-make-toolbar-from-menu (items menu-items map)
	    (let ((pointer items)
		  toolbar
		  file
		  menu-item
		  (riece-data-directory (locate-data-directory "riece")))
	      (while pointer
		(setq file (locate-file (symbol-name (car pointer))
					(if riece-data-directory
					    (cons riece-data-directory
						  load-path)
					  load-path)
					'(".xpm" ".pbm" ".xbm"))
		      menu-item (riece-toolbar-find-menu-item (car pointer)))
		(if (and file (file-exists-p file))
		    (setq toolbar
			  (toolbar-add-item
			   toolbar
			   (toolbar-new-button
			    file
			    (car pointer)
			    (if menu-item
				(aref menu-item 0)
			      (symbol-name (car pointer)))))))
		(setq pointer (cdr pointer)))
	      toolbar))
	  (defun riece-set-toolbar (toolbar)
	    (set-specifier default-toolbar toolbar (current-buffer))))
      (defalias 'riece-make-toolbar-from-menu 'ignore)
      (defalias 'riece-set-toolbar 'ignore))
  (defun riece-make-toolbar-from-menu (items menu-items map)
    (let ((pointer items)
	  (tool-bar-map (make-sparse-keymap)))
      (while pointer
	(tool-bar-add-item-from-menu (car pointer)
				     (symbol-name (car pointer))
				     map)
	(setq pointer (cdr pointer)))
      tool-bar-map))
  (defun riece-set-toolbar (toolbar)
    (make-local-variable 'tool-bar-map)
    (setq tool-bar-map toolbar)))

(defvar riece-command-mode-map)
(defun riece-toolbar-insinuate-in-command-buffer ()
  (riece-set-toolbar
   (riece-make-toolbar-from-menu
    riece-toolbar-items
    riece-menu-items
    riece-command-mode-map)))

(defun riece-toolbar-requires ()
  '(riece-menu))

(defun riece-toolbar-insinuate ()
  (add-hook 'riece-command-mode-hook
	    'riece-toolbar-insinuate-in-command-buffer
	    t))

(provide 'riece-toolbar)

;;; riece-toolbar.el ends here