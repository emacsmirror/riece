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

(defvar riece-channel-history nil)

(defun riece-guess-channel-from-history ()
  (let ((length (ring-length riece-channel-history))
	(index 0)
	result)
    (while (< index length)
      (setq result (cons (ring-ref riece-channel-history index) result)
	    index (1+ index)))
    (nreverse result)))

(defun riece-history-format-identity-for-channel-list-buffer (index identity)
  (if (and (not (ring-empty-p riece-channel-history))
	   (riece-identity-equal identity (ring-ref riece-channel-history 0)))
      (concat (format "%2d:+" index)
	      (riece-format-identity identity))))

(defun riece-history-format-identity-for-channel-list-indicator (index
								 identity)
  (if (and (not (ring-empty-p riece-channel-history))
	   (riece-identity-equal identity (ring-ref riece-channel-history 0)))
      (let ((string (riece-format-identity identity)))
	(put-text-property 0 (length string)
			   'face 'riece-channel-list-history-face
			   string)
	(concat (format "%d:" index) string))))

;;; (defun riece-history-requires ()
;;;   (if (memq 'riece-guess riece-addons)
;;;       '(riece-guess)))

(defun riece-history-insinuate ()
  (add-hook 'riece-startup-hook
	    (lambda ()
	      (setq riece-channel-history
		    (make-ring riece-channel-history-length))))
  (add-hook 'riece-exit-hook
	    (lambda ()
	      (setq riece-channel-history nil)))
  (add-hook 'riece-after-switch-to-channel-functions
	    (lambda (last)
	      (if (and last
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

(provide 'riece-history)

;;; riece-history.el ends here

