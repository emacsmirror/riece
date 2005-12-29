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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'riece-options)
(require 'riece-globals)
(require 'riece-identity)
(require 'riece-mode)
(require 'riece-cache)

;;; Channel object:
(defun riece-find-channel (name)
  "Get a channel object named NAME from the server buffer."
  (riece-cache-get riece-channel-cache name)
  (let ((symbol (intern-soft (riece-identity-canonicalize-prefix name)
			     riece-channel-obarray)))
    (if symbol
	(symbol-value symbol))))

(defun riece-forget-channel (name)
  (riece-cache-delete riece-channel-cache name)
  (let ((symbol (intern-soft (riece-identity-canonicalize-prefix name)
			     riece-channel-obarray)))
    (when symbol
      (makunbound symbol)
      (unintern (symbol-name symbol) riece-channel-obarray))))

(defun riece-make-channel (users topic modes banned invited uninvited key)
  "Make an instance of channel object.
Arguments are appropriate to channel users, topic, modes, banned
users, invited users, uninvited users, and the channel key,
respectively."
  (vector users topic modes banned invited uninvited))

(defun riece-get-channel (name)
  (let ((symbol (intern-soft (riece-identity-canonicalize-prefix name)
			     riece-channel-obarray)))
    (if symbol
	(progn
	  (riece-cache-get riece-channel-cache name)
	  (symbol-value symbol))
      (riece-cache-set riece-channel-cache name name)
      (set (intern (riece-identity-canonicalize-prefix name)
		   riece-channel-obarray)
	   (riece-make-channel nil nil nil nil nil nil nil)))))

(defun riece-channel-users (channel)
  "Return the users of CHANNEL."
  (aref channel 0))

(defun riece-channel-topic (channel)
  "Return the topic of CHANNEL."
  (aref channel 1))

(defun riece-channel-modes (channel)
  "Return the modes of CHANNEL."
  (aref channel 2))

(defun riece-channel-banned (channel)
  "Return the banned users of CHANNEL."
  (aref channel 3))

(defun riece-channel-invited (channel)
  "Return the invited users of CHANNEL."
  (aref channel 4))

(defun riece-channel-uninvited (channel)
  "Return the uninvited users of CHANNEL."
  (aref channel 5))

(defun riece-channel-key (channel)
  "Return the key of CHANNEL."
  (aref channel 6))

(defun riece-channel-set-users (channel value)
  "Set the users of CHANNEL to VALUE."
  (aset channel 0 value))

(defun riece-channel-set-topic (channel value)
  "Set the topic of CHANNEL to VALUE."
  (aset channel 1 value))

(defun riece-channel-set-modes (channel value)
  "Set the modes of CHANNEL to VALUE."
  (aset channel 2 value))

(defun riece-channel-set-banned (channel value)
  "Set the banned users of CHANNEL to VALUE."
  (aset channel 3 value))

(defun riece-channel-set-invited (channel value)
  "Set the invited users of CHANNEL to VALUE."
  (aset channel 4 value))

(defun riece-channel-set-uninvited (channel value)
  "Set the uninvited users of CHANNEL to VALUE."
  (aset channel 5 value))

(defun riece-channel-set-key (channel value)
  "Set the key of CHANNEL to VALUE."
  (aset channel 6 value))

(defun riece-channel-get-users (name)
  "Return channel's users as list."
  (riece-channel-users (riece-get-channel name)))

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
	 (modes (riece-channel-modes channel))
	 (old (riece-mode-assoc (riece-mode-flag mode) modes)))
    (if flag
	(unless old
	  (riece-channel-set-modes channel (cons mode modes)))
      (if old
	  (riece-channel-set-modes channel (delq old modes))))))

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
	(unless (riece-identity-assoc user users t)
	  (riece-channel-set-users channel (cons (list user) users)))
      (if (setq user (riece-identity-assoc user users t))
	  (riece-channel-set-users channel (delq user users))))))

(defun riece-channel-toggle-operator (name user flag)
  "Add or remove an operator to channel."
  (let* ((channel (riece-get-channel name))
	 (users (riece-channel-users channel)))
    (setq user (riece-identity-assoc user users t))
    (if flag
	(if user
	    (unless (memq ?o (cdr user))
	      (setcdr user (cons ?o (cdr user))))
	  (riece-channel-set-users channel (cons (list user ?o) users)))
      (if user
	  (setcdr user (delq ?o (cdr user)))))))

(defun riece-channel-toggle-speaker (name user flag)
  "Add or remove an speaker to channel."
  (let* ((channel (riece-get-channel name))
	 (users (riece-channel-users channel)))
    (setq user (riece-identity-assoc user users t))
    (if flag
	(if user
	    (unless (memq ?v (cdr user))
	      (setcdr user (cons ?v (cdr user))))
	  (riece-channel-set-users channel (cons (list user ?v) users)))
      (if user
	  (setcdr user (delq ?v (cdr user)))))))

(provide 'riece-channel)

;;; riece-channel.el ends here
