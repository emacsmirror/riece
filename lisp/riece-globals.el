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
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

;;; Miscellaneous global variables:
(defvar riece-server-process nil
  "Primary server process.")
(defvar riece-server-process-alist nil
  "An alist mapping secondary server name to opened processes.")

(defvar riece-current-channel nil
  "The channel you currently have joined.")
(defvar riece-current-channels nil
  "The channels you have currently joined.")

(defvar riece-save-variables-are-dirty nil
  "Non nil if the variables in `riece-saved-forms' are changed.")

(defvar riece-polling 0
  "Interval for polling the server.")

(defvar riece-reconnect-with-password nil
  "If non-nil, attempt to reconnect with password.")

(defvar riece-obarray-size 1327
  "The size of obarray used by riece on channelname and username space.")

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
(defvar riece-obarray nil
  "Namespace of the IRC world.
Local to the server buffers.")
(defvar riece-coding-system nil
  "Coding system for process I/O.
Local to the server buffers.")

;;; Variables local to the command buffer:
(defvar riece-default-channel-candidate nil
  "A channel name used as completion candidate.
Local to the command buffer.")
(defvar riece-last-channel nil
  "The channel you joined the last time.")
(defvar riece-command-buffer-mode 'channel
  "Command buffer mode.
Possible values are `chat' and `channel'.
Local to the command buffer.")

;;; Variables local to the channel buffers:
(defvar riece-freeze nil
  "If t, channel window is not scrolled.
If 'own, channel window is not scrolled until you speak.
Local to the channel buffers.")

;;; Modeline indicators:
(defvar riece-channel-indicator "None"
  "A modeline indicator of the current channel.")
(defvar riece-channel-list-indicator "No channel"
  "The current joined channels, \"pretty-printed.\".")
(defvar riece-user-indicator nil)

(defvar riece-away-indicator "-")
(defvar riece-operator-indicator "-")
(defvar riece-freeze-indicator "-")

;;; Buffers:
(defvar riece-command-buffer "*Commands*"
  "Name of command input buffer.")
(defvar riece-dialogue-buffer "*Dialogue*"
  "Name of dialogue output buffer.")
(defvar riece-private-buffer "*Private*"
  "Name of private message buffer.")
(defvar riece-others-buffer "*Others*"
  "Name of others message buffer.")
(defvar riece-channel-buffer nil
  "Name of channel message buffer.")
(defvar riece-channel-buffer-format "*Channel:%s*"
  "Format of channel message buffer.")
(defvar riece-channel-list-buffer " *Channels*"
  "Name of channel list buffer.")
(defvar riece-user-list-buffer nil
  "Name of user list buffer.")
(defvar riece-user-list-buffer-format " *Users:%s*"
  "Format of user list buffer.")
(defvar riece-wallops-buffer " *WALLOPS*")

(defvar riece-channel-buffer-alist nil)
(defvar riece-user-list-buffer-alist nil)
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
