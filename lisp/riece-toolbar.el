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

;; NOTE: This add-on doesn't support XEmacs yet.

;; To use, add the following line to your ~/.riece/init.el:
;; (add-to-list 'riece-addons 'riece-toolbar)

;;; Code:

(require 'riece-menu)

(defvar riece-toolbar-item-list
  '((riece-command-quit "riece-command-quit")
    (riece-command-join "riece-command-join")
    (riece-command-part "riece-command-part")
    (riece-command-next-channel "riece-command-next-channel")
    (riece-command-previous-channel "riece-command-previous-channel")
    (riece-command-change-window-layout "riece-command-change-window-layout")))

(if (fboundp 'tool-bar-local-item-from-menu)
    (defalias 'riece-tool-bar-local-item-from-menu
      'tool-bar-local-item-from-menu)
  (if (fboundp 'tool-bar-add-item-from-menu)
      (defun riece-tool-bar-local-item-from-menu (command icon in-map
							  &optional from-map
							  &rest props)
	"Define tool bar binding for COMMAND using the given ICON in \
keymap IN-MAP."
	(let ((tool-bar-map in-map))
	  (apply #'tool-bar-add-item-from-menu command icon from-map props)))
    (defalias 'riece-tool-bar-local-item-from-menu 'ignore)))

(defvar riece-command-mode-map)
(defun riece-toolbar-insinuate-in-command-buffer ()
  (when (boundp 'tool-bar-map)
    (make-local-variable 'tool-bar-map)
    (setq tool-bar-map
	  (let ((map (make-sparse-keymap))
		(pointer riece-toolbar-item-list))
	    (while pointer
	      (riece-tool-bar-local-item-from-menu (car (car pointer))
						   (nth 1 (car pointer))
						   map riece-command-mode-map)
	      (setq pointer (cdr pointer)))
	    map))))

(defun riece-toolbar-requires ()
  '(riece-menu))

(defun riece-toolbar-insinuate ()
  (add-hook 'riece-command-mode-hook
	    (lambda ()
	      (riece-toolbar-insinuate-in-command-buffer))
	    t))

(provide 'riece-toolbar)

;;; riece-toolbar.el ends here