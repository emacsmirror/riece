;;; riece-skk-kakutei.el --- add-on skk-kakutei
;; Copyright (C) 2003 TAKAHASHI Kaoru

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

;; This add-on deny SKK's sankaku send.

;; To use, add the following line to your ~/.riece/init.el:
;; (add-to-list 'riece-addons 'riece-skk-kakutei)

;;; Code:

(eval-when-compile (require 'riece))

(defvar riece-skk-kakutei-enabled nil)

(defconst riece-highlight-description
  "Deny SKK's sankaku send")

(defun riece-skk-kakutei-command-enter-message ()
  "Send the current line to the current channel."
  (interactive)
  (when (riece-skk-kakutei)
    (riece-command-enter-message)))

(defun riece-skk-kakutei-command-enter-message-as-notice ()
  "Send the current line to the current channel as NOTICE."
  (interactive)
  (when (riece-skk-kakutei)
    (riece-command-enter-message-as-notice)))

(defun riece-skk-kakutei ()
  "When required after-follow return `t'."
  (interactive)
  (cond ((or (not (boundp 'skk-mode)) (not skk-mode))
	 t)
	((and (boundp 'skk-henkan-mode) (not skk-henkan-mode))
	 t)
	((and (boundp 'skk-henkan-on) (not skk-henkan-on))
	 t)
	(skk-egg-like-newline
	 (skk-kakutei)
	 nil)
	(t
	 (skk-kakutei)
	 t)))

(defun riece-skk-kakutei-insinuate ()
  )

(defun riece-skk-kakutei-enable ()
  (riece-define-keys riece-command-mode-map
    "\r" riece-skk-kakutei-command-enter-message
    [(control return)] riece-skk-kakutei-command-enter-message-as-notice)
  (setq riece-skk-kakutei-enabled t))

(defun riece-skk-kakutei-disable ()
  (riece-define-keys riece-command-mode-map
    "\r" riece-command-enter-message
    [(control return)] riece-command-enter-message-as-notice)
  (setq riece-skk-kakutei-enabled nil))

(provide 'riece-skk-kakutei)

;;; riece-skk-kakutei.el ends here
