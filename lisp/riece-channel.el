;;; riece-channel.el --- a channel object
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

(require 'riece-options)
(require 'riece-globals)
(require 'riece-identity)

;;; String representation of a channel:
(defun riece-channel-p (string)
  "Return t if STRING is a channel.
\(i.e. it matches `riece-channel-regexp')"
  (string-match (concat "^" riece-channel-regexp) string))

;;; Channel object:
(defun riece-find-channel (name)
  "Get a channel object named NAME from the server buffer."
  (let ((symbol (intern-soft (riece-identity-canonicalize-prefix name)
			     riece-obarray)))
    (if symbol
	(symbol-value symbol))))

(defun riece-forget-channel (name)
  (let ((symbol (intern-soft (riece-identity-canonicalize-prefix name)
			     riece-obarray)))
    (when symbol
      (makunbound symbol)
      (unintern (symbol-name symbol) riece-obarray))))

(defun riece-make-channel (users operators speakers
				 topic modes banned invited uninvited
				 key)
  "Make an instance of channel object.
Arguments are appropriate to channel users, operators, speakers
\(+v), topic, modes, banned users, invited users, uninvited users, and
the channel key, respectively."
  (vector users operators speakers topic modes banned invited uninvited))

(defun riece-get-channel (name)
  (let ((symbol (intern-soft (riece-identity-canonicalize-prefix name)
			     riece-obarray)))
    (if symbol
	(symbol-value symbol)
      (set (intern (riece-identity-canonicalize-prefix name)
		   riece-obarray)
	   (riece-make-channel nil nil nil nil nil nil nil nil nil)))))

(defun riece-channel-users (channel)
  "Return the users of CHANNEL."
  (aref channel 0))

(defun riece-channel-operators (channel)
  "Return the operators of CHANNEL."
  (aref channel 1))

(defun riece-channel-speakers (channel)
  "Return the speakers of CHANNEL."
  (aref channel 2))

(defun riece-channel-topic (channel)
  "Return the topic of CHANNEL."
  (aref channel 3))

(defun riece-channel-modes (channel)
  "Return the modes of CHANNEL."
  (aref channel 4))

(defun riece-channel-banned (channel)
  "Return the banned users of CHANNEL."
  (aref channel 5))

(defun riece-channel-invited (channel)
  "Return the invited users of CHANNEL."
  (aref channel 6))

(defun riece-channel-uninvited (channel)
  "Return the uninvited users of CHANNEL."
  (aref channel 7))

(defun riece-channel-key (channel)
  "Return the key of CHANNEL."
  (aref channel 8))

(defun riece-channel-set-users (channel value)
  "Set the users of CHANNEL to VALUE."
  (aset channel 0 value))

(defun riece-channel-set-operators (channel value)
  "Set the operators of CHANNEL to VALUE."
  (aset channel 1 value))

(defun riece-channel-set-speakers (channel value)
  "Set the speakers of CHANNEL to VALUE."
  (aset channel 2 value))

(defun riece-channel-set-topic (channel value)
  "Set the topic of CHANNEL to VALUE."
  (aset channel 3 value))

(defun riece-channel-set-modes (channel value)
  "Set the modes of CHANNEL to VALUE."
  (aset channel 4 value))

(defun riece-channel-set-banned (channel value)
  "Set the banned users of CHANNEL to VALUE."
  (aset channel 5 value))

(defun riece-channel-set-invited (channel value)
  "Set the invited users of CHANNEL to VALUE."
  (aset channel 6 value))

(defun riece-channel-set-uninvited (channel value)
  "Set the uninvited users of CHANNEL to VALUE."
  (aset channel 7 value))

(defun riece-channel-set-key (channel value)
  "Set the key of CHANNEL to VALUE."
  (aset channel 8 value))

(defun riece-channel-get-users (name)
  "Return channel's users as list."
  (riece-channel-users (riece-get-channel name)))

(defun riece-channel-get-operators (name)
  "Return channel's operators as list."
  (riece-channel-operators (riece-get-channel name)))

(defun riece-channel-get-speakers (name)
  "Return channel's speakers as list."
  (riece-channel-speakers (riece-get-channel name)))

(defun riece-channel-get-topic (name)
  "Return channel's topic."
  (riece-channel-topic (riece-get-channel name)))

(defun riece-channel-get-modes (name)
  "Return channel's modes as list."
  (riece-channel-modes (riece-get-channel name)))

(defun riece-channel-get-banned (name)
  "Return channel's banned users as list."
  (riece-channel-banned (riece-get-channel name)))

(defun riece-channel-get-invited (name)
  "Return channel's invited users as list."
  (riece-channel-invited (riece-get-channel name)))

(defun riece-channel-get-uninvited (name)
  "Return channel's uninvited users as list."
  (riece-channel-uninvited (riece-get-channel name)))

(defun riece-channel-get-key (name)
  "Return channel's key."
  (riece-channel-key (riece-get-channel name)))

;;; Functions called from `riece-handle-mode-message':
(defun riece-channel-toggle-mode (name mode flag)
  "Add or remove channel MODE of channel."
  (let* ((channel (riece-get-channel name))
	 (modes (riece-channel-modes channel)))
    (if flag
	(unless (memq mode modes)
	  (riece-channel-set-modes channel (cons mode modes)))
      (if (memq mode modes)
	  (riece-channel-set-modes channel (delq mode modes))))))

(defun riece-channel-toggle-banned (name pattern flag)
  "Add or remove banned PATTERN of channel."
  (let* ((channel (riece-get-channel name))
	 (banned (riece-channel-banned channel)))
    (if flag
	(unless (member pattern banned)
	  (riece-channel-set-banned channel (cons pattern banned)))
      (if (setq pattern (car (member pattern banned)))
	  (riece-channel-set-banned channel (delq pattern banned))))))

(defun riece-channel-toggle-invited (name pattern flag)
  "Add or remove invited PATTERN of channel."
  (let* ((channel (riece-get-channel name))
	 (invited (riece-channel-invited channel)))
    (if flag
	(unless (member pattern invited)
	  (riece-channel-set-invited channel (cons pattern invited)))
      (if (setq pattern (car (member pattern invited)))
	  (riece-channel-set-invited channel (delq pattern invited))))))

(defun riece-channel-toggle-uninvited (name pattern flag)
  "Add or remove uninvited PATTERN to channel."
  (let* ((channel (riece-get-channel name))
	 (uninvited (riece-channel-uninvited channel)))
    (if flag
	(unless (member pattern uninvited)
	  (riece-channel-set-uninvited channel (cons pattern uninvited)))
      (if (setq pattern (car (member pattern uninvited)))
	  (riece-channel-set-uninvited
	   channel (delq pattern uninvited))))))

(defun riece-channel-toggle-user (name user flag)
  "Add or remove an user to channel."
  (let* ((channel (riece-get-channel name))
	 (users (riece-channel-users channel)))
    (if flag
	(unless (member user users)
	  (riece-channel-set-users channel (cons user users)))
      (if (setq user (car (member user users)))
	  (riece-channel-set-users channel (delq user users))))))

(defun riece-channel-toggle-operator (name user flag)
  "Add or remove an operator to channel."
  (let* ((channel (riece-get-channel name))
	 (operators (riece-channel-operators channel)))
    (if flag
	(unless (member user operators)
	  (riece-channel-set-operators channel (cons user operators)))
      (if (setq user (car (member user operators)))
	  (riece-channel-set-operators channel (delq user operators))))))

(defun riece-channel-toggle-speaker (name user flag)
  "Add or remove an speaker to channel."
  (let* ((channel (riece-get-channel name))
	 (speakers (riece-channel-speakers channel)))
    (if flag
	(unless (member user speakers)
	  (riece-channel-set-speakers channel (cons user speakers)))
      (if (setq user (car (member user speakers)))
	  (riece-channel-set-speakers channel (delq user speakers))))))

(provide 'riece-channel)

;;; riece-channel.el ends here
