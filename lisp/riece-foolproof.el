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

(defun riece-foolproof-command-send-message-function ()
  (when riece-foolproof-enabled
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

(defun riece-foolproof-enable ()
  (setq riece-foolproof-enabled t))

(defun riece-foolproof-disable ()
  (setq riece-foolproof-enabled nil))

(provide 'riece-foolproof)

;;; riece-foolproof.el ends here
