;;; riece-layout.el --- layout manager add-on
;; Copyright (C) 1998-2003 Daiki Ueno

;; Author: Daiki Ueno <ueno@unixuser.org>
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

;; This add-on allows you to switch window configurations by their names.

;; To use, add the following line to your ~/.riece/init.el:
;; (add-to-list 'riece-addons 'riece-layout)

;;; Code:

(require 'riece-display)

(defgroup riece-layout nil
  "Manage window layouts"
  :prefix "riece-"
  :group 'riece)

(defcustom riece-default-layout "default"
  "The default layout name."
  :type 'string
  :group 'riece-layout)

(defcustom riece-layout riece-default-layout
  "Current layout setting."
  :type 'string
  :group 'riece-layout)

(defcustom riece-layout-alist
  '(("default" riece-configure-windows riece-configure-windows-predicate)
    ("top" riece-configure-windows-top riece-configure-windows-predicate))
  "An alist mapping the names to configure/predicate functions."
  :type 'list
  :group 'riece-layout)

(defun riece-layout-option-set-function (symbol value)
  "Function called when setting `riece-layout'."
  (let ((layout (cdr (assoc value riece-layout-alist))))
    (unless layout
      (error "No such layout!"))
    (setq riece-configure-windows-function (car layout)
	  riece-configure-windows-predicate (nth 1 layout))
    (riece-redisplay-buffers t)))

(defun riece-command-change-layout (name)
  "Select a layout-name from all current available layouts and change
the layout to the selected layout-name."
  (interactive (list (completing-read "Layout: " riece-layout-alist)))
  (customize-set-variable 'riece-layout name))

(defvar riece-dialogue-mode-map)

(defun riece-layout-insinuate ()
  (define-key riece-dialogue-mode-map "\C-tl" #'riece-command-change-layout)
  ;; Delay setting `riece-layout' using riece-layout-option-set-function.
  (add-hook 'riece-startup-hook
	    (lambda ()
	      (put 'riece-layout 'custom-set
		   'riece-layout-option-set-function)
	      (riece-layout-option-set-function 'riece-layout riece-layout))))

(provide 'riece-layout)

;;; riece-layout.el ends here
