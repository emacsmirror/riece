;;; riece-foolproof.el --- channel miss killer
;; Copyright (C) 2004 TAKAHASHI Kaoru

;; Author: TAKAHASHI "beatmaria" Kaoru <kaoru@kaisei.org>
;; Keywords: IRC, riece

;; This file is part of Riece.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This add-on channel miss hold in check

;; To use, add the following line to your ~/.riece/init.el:
;; (add-to-list 'riece-addons 'riece-foolproof)

;;; Code:

(eval-when-compile
  (require 'riece-identity)
  (require 'riece-display))

(defvar riece-foolproof-enabled nil)

(defconst riece-foolproof-description
  "Channel miss killer")

(defun riece-foolproof-get-channel-window (identity)
  (get-buffer-window
   (cdr (riece-identity-assoc
	 identity riece-channel-buffer-alist))))

(eval-when-compile
  (defvar *dmacro-key* nil))

(defun riece-foolproof-dmacro-override (&optional arg)
  (when (and (fboundp 'dmacro-exec) (boundp '*dmacro-key*))
    (with-current-buffer riece-command-buffer
      (if arg
	  (when (eq (key-binding *dmacro-key*) 'dmacro-exec)
	    (local-set-key *dmacro-key* #'ignore))
	(when (eq (key-binding *dmacro-key*) 'ignore)
	  (local-unset-key *dmacro-key*))))))

(defun riece-foolproof-insinuate ()
  (defadvice riece-command-send-message (before riece-foolproof)
    (unless (or (not riece-channel-buffer-mode)
		(riece-foolproof-get-channel-window
		 riece-current-channel))
      (error "%s is not displayed. (maybe channel miss)"
	     (riece-identity-prefix riece-current-channel)))
    (unless (null executing-macro)
      (error "Don't use `riece-command-send-message' in keyboard macro"))))

(defun riece-foolproof-enable ()
  (riece-foolproof-dmacro-override t)
  (ad-enable-advice 'riece-command-send-message 'before 'riece-foolproof)
  (ad-activate 'riece-command-send-message)
  (setq riece-foolproof-enabled t))

(defun riece-foolproof-disable ()
  (riece-foolproof-dmacro-override nil)
  (ad-disable-advice 'riece-command-send-message 'before 'riece-foolproof)
  (ad-activate 'riece-command-send-message)
  (setq riece-foolproof-enabled nil))

(provide 'riece-foolproof)

;;; riece-foolproof.el ends here
