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

;; To use, add the following line to your ~/.riece/init.el:
;; (add-to-list 'riece-addons 'riece-history)

;;; Code:

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

(defvar riece-channel-history nil)

(defun riece-guess-channel-from-history ()
  (let ((length (ring-length riece-channel-history))
	(index 0)
	result)
    (while (< index length)
      (setq result (cons (ring-ref riece-channel-history index) result)
	    index (1+ index)))
    (nreverse result)))

(defun riece-history-requires ()
  (if (memq 'riece-guess riece-addons)
      '(riece-guess)))

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
	      (ring-insert riece-channel-history last)))
  (if (memq 'riece-guess riece-addons)
      (add-hook 'riece-guess-channel-try-functions
		'riece-guess-channel-from-history)))

(provide 'riece-history)

;;; riece-history.el ends here

