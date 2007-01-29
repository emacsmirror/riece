;;; riece-options.el --- customization
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

;;; Code:

(require 'riece-version)
(require 'riece-globals)

;; User modifiable variables.
(defgroup riece nil
  "Riece specific customize group."
  :prefix "riece-"
  :group 'applications)

(defgroup riece-options nil
  "Riece user customizable variables."
  :prefix "riece-"
  :group 'riece)

(defcustom riece-saved-forms
  '(riece-server-alist
    riece-channel-buffer-mode
    riece-others-buffer-mode
    riece-user-list-buffer-mode
    riece-channel-list-buffer-mode
    riece-layout
    riece-addons)
  "Variables saved after each session is completed."
  :type 'string
  :group 'riece-options)

(defcustom riece-debug nil
  "If non-nil, random debug spews."
  :type 'boolean
  :group 'riece-options)

(defcustom riece-command-prefix "\C-c"
  "Key sequence to be used as prefix for command mode key bindings."
  :type 'string
  :group 'riece-options)

(defgroup riece-looks nil
  "Look and feel."
  :prefix "riece-"
  :group 'riece)

(defcustom riece-use-full-window t
  "If non-nil, whole Emacs window is used to display dialogue."
  :type 'boolean
  :group 'riece-looks)

(defcustom riece-window-center-line -2
  "Line number of center point in window when scrolling.
If nil, erases the entire frame and then redraws with point in the
center of the window.  Negative means relative to bottom of window.
See the document of the function `recenter'."
  :type 'integer
  :group 'riece-looks)

(defcustom riece-directory (expand-file-name "~/.riece")
  "Where to look for startup files."
  :type 'directory
  :group 'riece-options)

(defcustom riece-addon-directory
  (expand-file-name "addons" riece-directory)
  "Where to look for add-on files."
  :type 'directory
  :group 'riece-options)

(defcustom riece-data-directory
  (if (fboundp 'locate-data-directory)
      (locate-data-directory "riece")
    (file-name-directory load-file-name))
  "Where to look for data files."
  :type 'directory
  :group 'riece-options)

(defcustom riece-variables-file
  (expand-file-name "init" riece-directory)
  "Where to look for variables."
  :type 'file
  :group 'riece-options)

(defcustom riece-saved-variables-file
  (expand-file-name "save" riece-directory)
  "Where to look for variables.
This file was saved the last session."
  :type 'file
  :group 'riece-options)

(defcustom riece-variables-files
  (list riece-saved-variables-file riece-variables-file)
  "Where to look for variables.  Helps to remove clutter from your .emacs.
This feature is most likely to dissappear in near future.  The preferred
way is to put Riece variables on .emacs or file loaded from there."
  :type '(repeat (file :tag "Initialization File"))
  :group 'riece-options)

(defcustom riece-addons '(riece-highlight
			  riece-ctcp
			  riece-guess
			  riece-unread
			  riece-history
			  riece-url
			  riece-button
			  riece-menu
			  riece-icon
			  riece-ignore
			  riece-log
			  riece-toolbar
			  riece-alias
			  riece-ctlseq
			  riece-keyword
			  riece-shrink-buffer
			  riece-mcat)
  "Add-ons insinuated into Riece."
  :type '(repeat symbol)
  :group 'riece-options)

(defgroup riece-server nil
  "Server settings."
  :prefix "riece-"
  :group 'riece)

(defgroup riece-channel nil
  "Channel settings."
  :prefix "riece-"
  :group 'riece)

(define-widget 'riece-service-spec 'radio
  "Edit service spec entries"
  :convert-widget 'riece-service-spec-convert)

(defun riece-service-spec-convert (widget)
  (widget-put widget :args '((integer :tag "Port Number")
			     (string :tag "Name")))
  widget)

(define-widget 'riece-server-spec 'repeat
  "Edit server spec entries"
  :match (lambda (widget value)
	   (eval `(and ,@(mapcar
			  (lambda (entry)
			    (or (stringp (cdr entry))
				(listp (cdr entry))))
			  value))))
  :convert-widget 'riece-server-spec-convert)

(defun riece-server-spec-convert (widget)
  (let* ((host '(const :format "" :value :host))
	 (service '(const :format "" :value :service))
	 (host
	  `(group :inline t ,host (string :tag "Host")))
	 (service
	  `(group :inline t ,service riece-service-spec))
	 (spec
	  `(cons (string :tag "Name")
		 (radio (string :tag "Host")
			(list ,host ,service))))
	 (args (list spec)))
    (widget-put widget :args args)
    widget))

(defcustom riece-server-alist nil
  "An alist mapping server names to plist."
  :type 'riece-server-spec
  :group 'riece-server)

(defcustom riece-server (getenv "IRCSERVER")
  "IRC server host we are connecting to."
  :type 'string
  :group 'riece-server)

(defcustom riece-protocol 'irc
  "Protocol support."
  :type 'symbol
  :group 'riece-server)

(defcustom riece-max-send-size 512
  "Maximum size of messages to be sent at a time."
  :type 'integer
  :group 'riece-server)

(defcustom riece-send-delay 2
  "Duration of multiple send."
  :type 'integer
  :group 'riece-server)
  
(defcustom riece-default-password (getenv "IRCPASSWORD")
  "Your password."
  :type '(radio (string :tag "Password")
		(const :tag "No" nil))
  :group 'riece-server)

(defcustom riece-username nil
  "Your login name."
  :type 'string
  :group 'riece-server)

(defcustom riece-realname nil
  "Your user name."
  :type 'string
  :group 'riece-server)

(defcustom riece-nickname (or (getenv "IRCNICK")
			      (user-real-login-name))
  "Your nickname."
  :type 'string
  :group 'riece-server)

(defcustom riece-startup-channel-list nil
  "A list of channels to join automatically at startup."
  :type '(repeat (choice (string :tag "Channel")
			 (list (string :tag "Channel") (string :tag "Key"))))
  :group 'riece-channel)

(defcustom riece-startup-server-list nil
  "A list of servers to connect automatically at startup."
  :type '(repeat (string :tag "Server"))
  :group 'riece-server)

(defcustom riece-retry-with-new-nickname nil
  "When nickname has already been in use, grow-tail automatically."
  :type 'boolean
  :group 'riece-server)

(defcustom riece-quit-timeout 1
  "Quit timeout when there is no response from server."
  :type '(radio (integer :tag "Seconds")
		(const nil))
  :group 'riece-server)

(defcustom riece-default-open-connection-function #'open-network-stream
  "Default function used for connecting to an IRC server."
  :type 'function
  :group 'riece-server)

(defcustom riece-user-cache-max-size 512
  "Maximum size of cache of user names."
  :type 'integer
  :group 'riece-server)

(defcustom riece-channel-cache-max-size 512
  "Maximum size of cache of channel names."
  :type 'integer
  :group 'riece-server)

(defcustom riece-channel-buffer-mode t
  "When non-nil, Riece will display a channel buffer."
  :type 'boolean
  :group 'riece-looks)

(defcustom riece-others-buffer-mode t
  "When non-nil, Riece will display an \"*Others*\" buffer."
  :type 'boolean
  :group 'riece-looks)

(defcustom riece-user-list-buffer-mode t
  "When non-nil, Riece will display a nick list buffer."
  :type 'boolean
  :group 'riece-looks)

(defcustom riece-channel-list-buffer-mode t
  "When non-nil, Riece will display a channel list buffer."
  :type 'boolean
  :group 'riece-looks)

(defcustom riece-default-freeze nil
  "Channel buffer local freeze flag is on at starting."
  :type 'boolean
  :group 'riece-looks)

(defcustom riece-default-channel-binding nil
  "The channel list to bind the channel number when joining."
  :type '(repeat (radio (string :tag "Bound Channel")
			(const nil)))
  :group 'riece-channel)

(defcustom riece-blink-parens nil
  "Should we blink matching parenthesis in the command buffer?"
  :type 'boolean
  :group 'riece-options)

(defcustom riece-quit-message (riece-extended-version)
  "Default quit message."
  :type '(string :tag "Quit message")
  :group 'riece-options)

(defcustom riece-part-message nil
  "Default part message."
  :type '(choice (const :tag "No message" nil)
		 (string :tag "Part message"))
  :group 'riece-options)

(defcustom riece-away-message "Gone"
  "Default away message."
  :type '(string :tag "Away message")
  :group 'riece-options)

(defcustom riece-gather-channel-modes nil
  "If non-nil, gather channel modes when we join a channel."
  :type 'boolean
  :group 'riece-options)

(defcustom riece-buffer-dispose-function #'kill-buffer
  "Function called after the buffer was disposed."
  :type 'function
  :group 'riece-options)

(defcustom riece-format-time-function #'current-time-string
  "Function to convert the specified time to the human readable form."
  :type 'function
  :group 'riece-options)

(provide 'riece-options)

;;; riece-options.el ends here
