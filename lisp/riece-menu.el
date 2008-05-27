;;; riece-menu.el --- setup menus
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
(require 'riece-globals)
(require 'riece-identity)
(require 'riece-layout)
(require 'riece-server)
(require 'riece-mcat)

(defcustom riece-menu-items
  (list
   "Riece"
   (vector (riece-mcat "Next Channel") 'riece-command-next-channel
	   'riece-current-channels)
   (vector (riece-mcat "Previous Channel") 'riece-command-previous-channel
	   'riece-current-channels)
   "----"
   (list (riece-mcat "Channels")
	 :filter 'riece-menu-create-channels-menu)
   (list (riece-mcat "Servers")
	 :filter 'riece-menu-create-servers-menu)
   "----"
   (vector (riece-mcat "Redraw Layout") 'riece-command-configure-windows t)
   (list (riece-mcat "Change Window Layout...")
	 :filter 'riece-menu-create-layouts-menu)
   (list (riece-mcat "Toggle...")
	 (vector (riece-mcat "Freeze Channel Buffer")
		 'riece-command-toggle-freeze t)
	 (vector (riece-mcat "Freeze Channel Buffer Until Next Message")
		 'riece-command-toggle-own-freeze t)
	 (vector (riece-mcat "Display Channel Buffer")
		 'riece-command-toggle-channel-buffer-mode t)
	 (vector (riece-mcat "Display Channel List Buffer")
		 'riece-command-toggle-channel-list-buffer-mode t)
	 (vector (riece-mcat "Display User List Buffer")
		 'riece-command-toggle-user-list-buffer-mode t))
   "----"
   (vector (riece-mcat "Join Channel") 'riece-command-join t)
   (vector (riece-mcat "Part Channel") 'riece-command-part
	   'riece-current-channel)
   (vector (riece-mcat "Set Channel Topic") 'riece-command-topic
	   'riece-current-channel)
   (vector (riece-mcat "Kick User") 'riece-command-kick
	   'riece-current-channel)
   (vector (riece-mcat "Invite User") 'riece-command-invite
	   'riece-current-channel)
   "----"
   (vector (riece-mcat "Manage Add-ons") 'riece-command-list-addons t)
   (vector (riece-mcat "Version") 'riece-version t)
   (vector (riece-mcat "Submit Bug Report") 'riece-submit-bug-report t)
   "----"
   (vector (riece-mcat "Mark As Away") 'riece-command-toggle-away t)
   (vector (riece-mcat "Change Nickname") 'riece-command-change-nickname t)
   (vector (riece-mcat "Quit IRC") 'riece-command-quit t))
  "Menu used in command mode."
  :type 'sexp
  :group 'riece-options)

(defconst riece-menu-description
  "Setup Riece's command menus.")

(defun riece-menu-create-layouts-menu (menu)
  (mapcar (lambda (entry)
	    (vector (car entry) (list 'riece-command-change-layout (car entry))
		    t))
	  riece-layout-alist))

(defun riece-menu-create-channels-menu (menu)
  (delq nil
	(mapcar (lambda (channel)
		  (if channel
		      (list (riece-format-identity channel)
			    (vector (riece-mcat "Switch To Channel")
				    (list 'riece-command-switch-to-channel
					  channel)
				    t)
			    (vector (riece-mcat "Part Channel")
				    (list 'riece-command-part channel) t)
			    (vector (riece-mcat "List Channel")
				    (list 'riece-command-list
					  (riece-identity-prefix channel)) t))))
		riece-current-channels)))

(defun riece-menu-create-servers-menu (menu)
  (mapcar (lambda (entry)
	    (list (car entry)
		  (vector (riece-mcat "Open Server")
			  (list 'riece-command-open-server (car entry))
			  (not (riece-server-opened (car entry))))
		  (vector (riece-mcat "Close Server")
			  (list 'riece-command-close-server (car entry))
			  (riece-server-opened (car entry)))))
	  riece-server-alist))

(defvar riece-command-mode-map)
(defvar riece-menu)

(defun riece-menu-command-mode-hook ()
  (easy-menu-define riece-menu
		    riece-command-mode-map
		    "Riece Menu"
		    riece-menu-items)
  (easy-menu-add riece-menu))

(defun riece-menu-requires ()
  (if (memq 'riece-mcat riece-addons)
      '(riece-mcat)))

(defun riece-menu-insinuate ()
  (if (fboundp 'custom-reevaluate-setting)
      (custom-reevaluate-setting 'riece-menu-items)
    ;; We could emulate custom-reevaluate-setting by manually eval the
    ;; 'standard-value property of 'riece-menu-items.  In that case
    ;; (i.e. XEmacs 21.4), however, there is no way to render
    ;; non-ASCII labels in the menu, so give up here.
    )
  (if riece-command-buffer
      (with-current-buffer riece-command-buffer
	(riece-menu-command-mode-hook)))
  (add-hook 'riece-command-mode-hook
	    'riece-menu-command-mode-hook))

(defun riece-menu-uninstall ()
  (if riece-command-buffer
      (with-current-buffer riece-command-buffer
	(easy-menu-remove riece-menu)))
  (remove-hook 'riece-command-mode-hook
	       'riece-menu-command-mode-hook))

(provide 'riece-menu)

;;; riece-menu.el ends here
