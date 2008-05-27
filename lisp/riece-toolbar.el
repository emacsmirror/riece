;;; riece-toolbar.el --- display toolbar icons
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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Image files are taken from stock icons:

;; riece-command-next-channel.xpm	gtk-go-forward
;; riece-command-previous-channel.xpm	gtk-go-back
;; riece-command-configure-windows.xpm	gtk-refresh
;; riece-command-list-addons		gtk-preferences
;; riece-command-join			gtk-new
;; riece-command-part			gtk-close

;; NOTE: This is an add-on module for Riece.

;;; Code:

(require 'riece-menu)

(defconst riece-toolbar-description
  "Display toolbar icons.")

(defvar riece-toolbar-items
  '((riece-command-previous-channel . "left-arrow")
    (riece-command-next-channel . "right-arrow")
    (riece-command-configure-windows . "refresh")
    (riece-command-join . "new")
    (riece-command-part . "close")
    (riece-command-list-addons . "preferences")))

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

(eval-and-compile
  (if (featurep 'xemacs)
      (if (featurep 'toolbar)
	  (progn
	    (defun riece-make-toolbar-from-menu (items menu-items map)
	      (let ((pointer items)
		    toolbar
		    file
		    menu-item)
		(while pointer
		  (setq file (locate-file (symbol-name (car (car pointer)))
					  (cons riece-data-directory load-path)
					  '(".xpm" ".pbm" ".xbm"))
			menu-item (riece-toolbar-find-menu-item
				   (car (car pointer))))
		  (if (and file (file-exists-p file))
		      (setq toolbar
			    (toolbar-add-item
			     toolbar
			     (toolbar-new-button
			      file
			      (car (car pointer))
			      (if menu-item
				  (aref menu-item 0)
				(symbol-name (car (car pointer))))))))
		  (setq pointer (cdr pointer)))
		toolbar))
	    (defvar riece-toolbar-original-toolbar nil)
	    (defun riece-set-toolbar (toolbar)
	      (make-local-variable 'riece-toolbar-original-toolbar)
	      (setq riece-toolbar-original-toolbar
		    (specifier-specs default-toolbar (current-buffer)))
	      (set-specifier default-toolbar toolbar (current-buffer)))
	    (defun riece-unset-toolbar ()
	      (if riece-toolbar-original-toolbar
		  (set-specifier default-toolbar riece-toolbar-original-toolbar
				 (current-buffer))
		(remove-specifier default-toolbar (current-buffer)))
	      (kill-local-variable 'riece-toolbar-original-toolbar)))
	(defalias 'riece-make-toolbar-from-menu 'ignore)
	(defalias 'riece-set-toolbar 'ignore)
	(defalias 'riece-unset-toolbar 'ignore))
    (defun riece-make-toolbar-from-menu (items menu-items map)
      (let ((pointer items)
	    (tool-bar-map (make-sparse-keymap)))
	(while pointer
	  (tool-bar-add-item-from-menu (car (car pointer))
				       (cdr (car pointer))
				       map)
	  (setq pointer (cdr pointer)))
	tool-bar-map))
    (defun riece-set-toolbar (toolbar)
      (make-local-variable 'tool-bar-map)
      (setq tool-bar-map toolbar))
    (defun riece-unset-toolbar ()
      (kill-local-variable 'tool-bar-map))))

(defvar riece-command-mode-map)
(defun riece-toolbar-command-mode-hook ()
  (riece-set-toolbar
   (riece-make-toolbar-from-menu
    riece-toolbar-items
    riece-menu-items
    riece-command-mode-map)))

(defun riece-toolbar-requires ()
  '(riece-menu))

(defun riece-toolbar-insinuate ()
  (if riece-command-buffer
      (with-current-buffer riece-command-buffer
	(riece-toolbar-command-mode-hook)))
  (add-hook 'riece-command-mode-hook
	    'riece-toolbar-command-mode-hook t))

(defun riece-toolbar-uninstall ()
  (if riece-command-buffer
      (with-current-buffer riece-command-buffer
	(riece-unset-toolbar)))
  (remove-hook 'riece-command-mode-hook
	       'riece-toolbar-command-mode-hook))

(provide 'riece-toolbar)

;;; riece-toolbar.el ends here
