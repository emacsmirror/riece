;;; riece-history.el --- channel history management add-on
;; Copyright (C) 1998-2003 Daiki Ueno

;; Author: Daiki Ueno <ueno@unixuser.org>
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

;;; Commentary:

;; You can check recently visited channels via `C-c g' in the commands
;; buffer, by adding the following lines to ~/.riece/init.el:

;;   (add-hook 'riece-guess-channel-try-functions
;;             'riece-guess-channel-from-history)

;;; Code:

(require 'riece-options)
(require 'riece-globals)
(require 'riece-highlight)
(require 'riece-identity)
(require 'riece-signal)
(require 'ring)

(defgroup riece-history nil
  "Channel history"
  :tag "History"
  :prefix "riece-"
  :group 'riece)

(defcustom riece-channel-history-length 3
  "Length of riece-channel-history."
  :type 'integer
  :group 'riece-history)

(defface riece-channel-list-history-face
  '((((class color)
      (background dark))
     (:foreground "PaleTurquoise"))
    (((class color)
      (background light))
     (:foreground "SeaGreen3"))
    (t
     (:bold t)))
  "Face used for displaying history channels."
  :group 'riece-highlight-faces)
(defvar riece-channel-list-history-face 'riece-channel-list-history-face)

(unless (riece-facep 'riece-modeline-history-face)
  (make-face 'riece-modeline-history-face
	     "Face used for displaying history channels in modeline.")
  (if (featurep 'xemacs)
      (set-face-parent 'riece-modeline-history-face 'modeline))
  (set-face-foreground 'riece-modeline-history-face
		       (face-foreground 'riece-channel-list-history-face)))

(defvar riece-modeline-history-face 'riece-modeline-history-face)

(defvar riece-channel-history nil)

(defvar riece-history-enabled nil)

(defconst riece-history-description
  "Keep track channel history")

(defun riece-guess-channel-from-history ()
  (let ((length (ring-length riece-channel-history))
	(index 0)
	result)
    (while (< index length)
      (setq result (cons (ring-ref riece-channel-history index) result)
	    index (1+ index)))
    (nreverse result)))

(defun riece-history-format-identity-for-channel-list-buffer (index identity)
  (if (and riece-history-enabled
	   (not (ring-empty-p riece-channel-history))
	   (riece-identity-equal identity (ring-ref riece-channel-history 0)))
      (concat (format "%2d:+" index)
	      (riece-format-identity identity))))

(defun riece-history-format-identity-for-channel-list-indicator (index
								 identity)
  (if (and riece-history-enabled
	   (not (ring-empty-p riece-channel-history))
	   (riece-identity-equal identity (ring-ref riece-channel-history 0)))
      (let ((string (riece-format-identity identity))
	    (start 0))
	;; Escape % -> %%.
	(while (string-match "%" string start)
	  (setq start (1+ (match-end 0))
		string (replace-match "%%" nil nil string)))
	(list (format "%d:" index)
	      (riece-propertize-modeline-string
	       string 'face 'riece-modeline-history-face)))))

;;; (defun riece-history-requires ()
;;;   (if (memq 'riece-guess riece-addons)
;;;       '(riece-guess)))

(defun riece-history-insinuate ()
  (add-hook 'riece-after-switch-to-channel-functions
	    (lambda (last)
	      (if (and riece-history-enabled last
		       (not (riece-identity-equal last riece-current-channel)))
		  (ring-insert riece-channel-history last))))
  (add-hook 'riece-format-identity-for-channel-list-buffer-functions
	    'riece-history-format-identity-for-channel-list-buffer)
  (add-hook 'riece-format-identity-for-channel-list-indicator-functions
	    'riece-history-format-identity-for-channel-list-indicator)
  (if (memq 'riece-highlight riece-addons)
      (setq riece-channel-list-mark-face-alist
	    (cons '(?+ . riece-channel-list-history-face)
		  riece-channel-list-mark-face-alist)))
;;;  (if (memq 'riece-guess riece-addons)
;;;      (add-hook 'riece-guess-channel-try-functions
;;;		'riece-guess-channel-from-history))
  )

(defun riece-history-enable ()
  (setq riece-channel-history
	(make-ring riece-channel-history-length))
  (setq riece-history-enabled t)
  (riece-emit-signal 'channel-list-changed))

(defun riece-history-disable ()
  (setq riece-channel-history nil
	riece-history-enabled nil)
  (riece-emit-signal 'channel-list-changed))

(provide 'riece-history)

;;; riece-history.el ends here
