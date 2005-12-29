;;; riece-menu.el --- setup Riece's command menus
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

(defvar riece-menu-items
  `("Riece"
    ["Version" riece-version t]
    ["Submit Bug Report" riece-submit-bug-report t]
    "----"
    ("Change Window Layout..." :filter riece-menu-create-layouts-menu)
    ["Toggle Freeze Channel Buffer"
     riece-command-toggle-freeze t]
    ["Toggle Freeze Channel Buffer Until Next Message"
     riece-command-toggle-own-freeze t]
    ["Toggle Display Channel Buffer"
     riece-command-toggle-channel-buffer-mode t]
    ["Toggle Display Channel List Buffer"
     riece-command-toggle-channel-list-buffer-mode t]
    ["Toggle Display User List Buffer"
     riece-command-toggle-user-list-buffer-mode t]
    "----"
    ["Join Channel" riece-command-join t]
    ["Change Nickname" riece-command-change-nickname t]
    ["Mark As Away" riece-command-toggle-away t]
    ["Quit IRC" riece-command-quit t]
    "----"
    ["Part Channel" riece-command-part riece-current-channel]
    ["Set Channel Topic" riece-command-topic riece-current-channel]
    ["Kick User" riece-command-kick riece-current-channel]
    ["Invite User" riece-command-invite riece-current-channel]
    "----"
    ["Next Channel" riece-command-next-channel riece-current-channels]
    ["Previous Channel" riece-command-previous-channel riece-current-channels]
    "----"
    ("Channels" :filter riece-menu-create-channels-menu)
    ("Servers" :filter riece-menu-create-servers-menu))
  "Menu used in command mode.")

(defconst riece-menu-description
  "Setup Riece's command menus.")

(defun riece-menu-create-layouts-menu (menu)
  (mapcar (lambda (entry)
	    (vector (car entry) (list 'riece-command-change-layout (car entry))
		    t))
	  riece-layout-alist))

(defun riece-menu-create-channels-menu (menu)
  (mapcar (lambda (channel)
	    (list (riece-format-identity channel)
		  (vector "Switch To Channel"
			  (list 'riece-command-switch-to-channel channel) t)
		  (vector "Part Channel"
			  (list 'riece-command-part channel) t)
		  (vector "List Channel"
			  (list 'riece-command-list
				(riece-identity-prefix channel)) t)))
	  riece-current-channels))

(defun riece-menu-create-servers-menu (menu)
  (mapcar (lambda (entry)
	    (list (car entry)
		  (vector "Open Server"
			  (list 'riece-command-open-server (car entry))
			  (not (riece-server-opened (car entry))))
		  (vector "Close Server"
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

(defun riece-menu-insinuate ()
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
