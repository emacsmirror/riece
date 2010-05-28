;;; riece-globals.el --- global variables and constants.
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

(require 'riece-compat)			;riece-make-interval-regexp

;;; Constants:
(defconst riece-strict-channel-regexp
  (concat "\\([+&#]\\|!"
	  (riece-make-interval-regexp "[A-Z0-9]" 5)
	  "\\|!!\\)[^\0\7\r\n ,:]*\\(:[^\0\7\r\n ,:]*\\)?"))

(defconst riece-strict-user-regexp
  (concat "[][\\\\`_^{|}A-Za-z]"
	  (riece-make-interval-regexp "[][\\\\`_^{|}A-Za-z0-9-]" 0 8)))

(defconst riece-laxed-channel-regexp
  "[+&#!][^\0\7\r\n ,:]*\\(:[^\0\7\r\n ,:]*\\)?")

(defconst riece-laxed-user-regexp
  "[][\\\\`_^{|}A-Za-z][][\\\\`_^{|}A-Za-z0-9-]*")

(defvar riece-channel-regexp riece-laxed-channel-regexp)
(defvar riece-user-regexp riece-laxed-user-regexp)

;;; Global variables:
(defvar riece-server-process-alist nil
  "An alist mapping server names to processes.")

(defvar riece-current-channel nil
  "The channel you currently have joined.")
(defvar riece-current-channels nil
  "The channels you have currently joined.")
(defvar riece-join-channel-candidate nil
  "The candidate for channel to be used with the next join command.")

(defvar riece-save-variables-are-dirty nil
  "Non nil if the variables in `riece-saved-forms' are changed.")

(defvar riece-polling 0
  "Interval for polling the server.")

(defvar riece-reconnect-with-password nil
  "If non-nil, attempt to reconnect with password.")

(defvar riece-user-obarray-size 1327
  "The size of obarray used by riece on user name space.")

(defvar riece-channel-obarray-size 103
  "The size of obarray used by riece on channel name space.")

(defvar riece-addon-dependencies nil)

;;; Variables local to the server buffers:
(defvar riece-server-name nil
  "The name of the server.
Local to the server buffers.")
(defvar riece-real-nickname nil
  "Your nickname the server offers.
Local to the server buffers.")
(defvar riece-last-nickname nil
  "The last nickname you requested.
Local to the server buffers.")
(defvar riece-nick-accepted nil
  "The flag your nickname is accepted by the server.
Possible values are nil, `ok', and `sent'.
Local to the server buffers.")
(defvar riece-real-server-name nil
  "The server name offered by the server.
Local to the server buffers.")
(defvar riece-real-userhost nil
  "Your hostname the server offers.
Local to the server buffers.")
(defvar riece-user-at-host ""
  "The user@host for the current input.
Local to the server buffers.")
(defvar riece-user-at-host-type nil
  "The authentication type of `riece-user-at-host'.
Possible values are 'ok 'not-verified 'fake or 'invalid.
Local to the server buffers.")
(defvar riece-supported-user-modes nil
  "User modes supported by server.
Local to the server buffers.")
(defvar riece-supported-channel-modes nil
  "Channel modes supported by server.
Local to the server buffers.")
(defvar riece-channel-filter ""
  "Filter of the result of NAMES or LIST.
This enables us to use \\[universal-argument] with NAMES and TOPIC.
Local to the server buffers.")
(defvar riece-read-point nil
  "Point at the last input was seen.
Local to the server buffers.")
(defvar riece-filter-running nil
  "Lock of the process filter; non-nil indicates the process filter is running.
Local to the server buffers.")
(defvar riece-send-queue nil
  "Send queue for avoiding client flood.
Local to the server buffers.")
(defvar riece-send-size nil
  "Size of the last send.
Local to the server buffers.")
(defvar riece-last-send-time nil
  "Timestamp of the last send.
Local to the server buffers.")
(defvar riece-user-obarray nil
  "USER namespace of the IRC world.
Local to the server buffers.")
(defvar riece-channel-obarray nil
  "Channel namespace of the IRC world.
Local to the server buffers.")
(defvar riece-coding-system nil
  "Coding system for process I/O.
Local to the server buffers.")
(defvar riece-channel-cache nil
  "Cache of channel names.
Local to the server buffers.")
(defvar riece-user-cache nil
  "Cache of user names.
Local to the server buffers.")

;;; Variables local to the channel buffers:
(defvar riece-freeze nil
  "If t, channel window is locked and will not be scrolled.
If 'own, channel window is locked until the user begins to speak.
Local to the channel buffers.")

(defvar riece-freeze-indicator nil
  "String displayed on the modeline to allow the user to tell if the
channel buffer is locked.
Local to the channel buffers.")

(defvar riece-channel-buffer-window-point nil
  "Last value of point in window which displayed the channel buffer.
Local to the channel buffers.")

;;; Modeline indicators:
(defvar riece-mode-line-buffer-identification nil)
(put 'riece-mode-line-buffer-identification 'risky-local-variable t)
(defvar riece-channel-indicator "None"
  "String displayed on the modeline to indicate the current channel.")
(put 'riece-channel-indicator 'risky-local-variable t)
(defvar riece-long-channel-indicator "None"
  "String displayed on the modeline to indicate the current channel.
Generally, this string will contain more information than
riece-channel-indicator.")
(put 'riece-long-channel-indicator 'risky-local-variable t)
(defvar riece-channel-list-indicator "No channel"
  "String displayed on the modeline to show the joined channels.")
(put 'riece-channel-list-indicator 'risky-local-variable t)
(defvar riece-user-indicator nil
  "String displayed on the modeline to show the current nickname.")
(put 'riece-user-indicator 'risky-local-variable t)

(defvar riece-away-indicator "-"
  "String displayed on the modeline to allow the user to tell if the
user is away.")
(put 'riece-away-indicator 'risky-local-variable t)
(defvar riece-operator-indicator "-"
  "String displayed on the modeline to allow the user to tell if the
user is an operator.")
(put 'riece-operator-indicator 'risky-local-variable t)
(defvar riece-channel-status-indicator "-"
  "String displayed on the modeline to allow the user to tell if the
user's status on the current channel.")
(put 'riece-channel-status-indicator 'risky-local-variable t)

;;; Buffers:
(defvar riece-command-buffer nil
  "The command buffer.")
(defvar riece-dialogue-buffer nil
  "Buffer for whole conversation.")
(defvar riece-others-buffer nil
  "Buffer for other messages.")
(defvar riece-channel-list-buffer nil
  "Buffer for channel list.")
(defvar riece-user-list-buffer nil
  "Buffer for user list.")
(defvar riece-channel-buffer nil
  "Buffer for messages arrived in the current channel.")
(defvar riece-temp-buffer nil
  "Buffer for temporally use.")
(defvar riece-debug-buffer nil
  "Buffer for debug output.")

(defvar riece-buffer-list nil)
(defvar riece-overriding-server-name nil)

(defconst riece-change-prefix "*** Change: ")
(defconst riece-notice-prefix "*** Notice: ")
(defconst riece-wallops-prefix "*** Notice: ")
(defconst riece-error-prefix "*** Error: ")
(defconst riece-info-prefix "*** Info: ")
(defconst riece-prefix-regexp "\\*\\*\\* \\([^:]+: \\)")

(defconst riece-time-prefix-regexp "[0-9][0-9]:[0-9][0-9] ")

(provide 'riece-globals)

;;; riece-globals.el ends here
