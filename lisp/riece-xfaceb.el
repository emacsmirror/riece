;;; riece-xfaceb.el --- display X-Face/Colour Face in IRC buffers
;; Copyright (C) 2005 Daiki Ueno

;; Author: Steve Youngs <steve@sxemacs.org>
;; Created: 2005-09-03
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

;;; Commentary:

;; NOTE: This is an add-on module for Riece.  It is adapted from
;; `riece-xface' but gets the image data from BBDB instead of LSDB.

;; This add-on displays either X-Face or colour Face images in the
;; Riece "Users" buffers.  The image data comes from entries in a BBDB
;; db.  Consequently it does require a little setting up in BBDB...

;; BBDB Setup:

;; You need a new field called "ircnick" for each IRC contact that is
;; in your BBDB.  Its value is the IRC nickname of the contact (what
;; is listed in the Riece "Users" buffer).

;;   M-x bbdb-insert-new-field RET ircnick RET
;;   answer `yes' to the prompt about the field not being defined
;;   then give it a value which will be that record's IRC nickname

;; Then you'll need to collect X-Face: and Face: headers from your mail.
;; To do that see: <http://www.emacswiki.org/cgi-bin/wiki/BbdbFaces>

;;; Code:

(require 'riece-identity)
(require 'riece-globals)
(require 'riece-display)

(autoload 'bbdb-records "bbdb")
(autoload 'bbdb-record-getprop "bbdb")

(defconst riece-xfaceb-description
  "Display X-Face & Colour Face images in IRC buffers \(BBDB\).")

(defcustom riece-xfaceb-prefer-cface-to-xface (featurep 'png)
  "*When non-nil, display colour face images instead of X-Face."
  :type 'boolean
  :group 'riece-looks)

(defun riece-xfaceb-face-to-png (face)
  "Base64 decode a Face header into a PNG.
Returns a string."
  (with-temp-buffer
    (insert face)
    (base64-decode-region (point-min) (point-max))
    (buffer-string)))

(defun riece-xfaceb-update-user-list-buffer ()
  "Add X-Face or Colour Face images to channel users' buffer."
  (when (get 'riece-xfaceb 'riece-addon-enabled)
    (let ((users (ignore-errors 
		   (riece-with-server-buffer
		       (riece-identity-server riece-current-channel)
		     (riece-channel-get-users (riece-identity-prefix
					       riece-current-channel)))))
	  all-records cface xface nick name record)
      (while users
	(setq name (caar users))
	(setq all-records (bbdb-records))
	(while all-records
	  (setq record (car all-records)
		nick (bbdb-record-getprop record 'ircnick)
		xface (bbdb-record-getprop record 'face)
		cface (bbdb-record-getprop record 'cface))
	  (when (and (equal nick name)
		     (or xface cface))
	    (with-current-buffer riece-user-list-buffer
	      (goto-char (point-min))
	      (re-search-forward (regexp-quote name) nil t)
	      (beginning-of-line)
	      (when (and xface
			 (or (not riece-xfaceb-prefer-cface-to-xface)
			     (not cface)))
		(set-extent-begin-glyph
		 (extent-at (point))
		 (make-glyph (list (vector 'xface
					   :data (concat "X-Face: " xface)
					   :foreground "black"
					   :background "white")))))
	      (when (and (featurep 'png)
			 riece-xfaceb-prefer-cface-to-xface
			 cface)
		(set-extent-begin-glyph
		 (extent-at (point))
		 (make-glyph (list (vector 'png
					   :data (riece-xfaceb-face-to-png cface)))))))
	    ;; We have a match, get out of the inner loop
	    (setq all-records nil))
	  (setq all-records (cdr all-records)))
	(setq users (cdr users))))))

(defun riece-xfaceb-requires ()
  )

(defun riece-xfaceb-user-list-mode-hook ()
  (add-hook 'riece-update-buffer-functions
	    'riece-xfaceb-update-user-list-buffer t t))

(defun riece-xfaceb-insinuate ()
  (if riece-user-list-buffer
      (with-current-buffer riece-user-list-buffer
	(riece-xfaceb-user-list-mode-hook)))
  (add-hook 'riece-user-list-mode-hook
	    'riece-xfaceb-user-list-mode-hook))

(defun riece-xfaceb-uninstall ()
  (if riece-user-list-buffer
      (with-current-buffer riece-user-list-buffer
	(remove-hook 'riece-update-buffer-functions
		     'riece-xfaceb-update-user-list-buffer t)))
  (remove-hook 'riece-user-list-mode-hook
	       'riece-xfaceb-user-list-mode-hook))

(defun riece-xfaceb-enable ()
  (if riece-current-channel
      (riece-emit-signal 'user-list-changed riece-current-channel)))

(defun riece-xfaceb-disable ()
  (if riece-current-channel
      (riece-emit-signal 'user-list-changed riece-current-channel)))

(provide 'riece-xfaceb)

;;; riece-xfaceb.el ends here

