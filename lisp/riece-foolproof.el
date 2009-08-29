;;; riece-foolproof.el --- prevent miss-operation in the command buffer
;; Copyright (C) 2004 TAKAHASHI Kaoru

;; Author: TAKAHASHI Kaoru <kaoru@kaisei.org>
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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; NOTE: This is an add-on module for Riece.

;;; Code:

(eval-when-compile
  (require 'riece-identity)
  (require 'riece-display))

(defconst riece-foolproof-description
  "Prevent miss-operation in the command buffer.")

(defun riece-foolproof-get-channel-window (identity)
  (get-buffer-window
   (cdr (riece-identity-assoc
	 identity riece-channel-buffer-alist))))

(defun riece-foolproof-command-send-message-function ()
  (when (get 'riece-foolproof 'riece-addon-enabled)
    (unless (or (not riece-channel-buffer-mode)
		(riece-foolproof-get-channel-window
		 riece-current-channel))
      (error "Channel %s is not displayed"
	     (riece-identity-prefix riece-current-channel)))
    (when (text-property-not-all
	   (riece-line-beginning-position) (riece-line-end-position)
	   'invisible nil)
      (error "Invisible text included: %s"
	     (buffer-substring-no-properties
	      (riece-line-beginning-position)
	      (riece-line-end-position))))
    (when executing-kbd-macro
      (error "%s" "Forbidden to run keyboard macro"))))

(defun riece-foolproof-insinuate ()
  (add-hook 'riece-command-send-message-hook
	    'riece-foolproof-command-send-message-function))

(defun riece-foolproof-uninstall ()
  (remove-hook 'riece-command-send-message-hook
	       'riece-foolproof-command-send-message-function))

(provide 'riece-foolproof)

;;; riece-foolproof.el ends here
