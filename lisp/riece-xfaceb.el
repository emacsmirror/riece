;;; riece-xfaceb.el --- display X-Face/Colour Face in IRC buffers -*- lexical-binding: t -*-
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

;; `bbdb-search' is defined as a macro in bbdb-com.
(eval-when-compile
  (condition-case nil
      (require 'bbdb-com)
    (error)))

(defconst riece-xfaceb-description
  "Display X-Face & Colour Face images in IRC buffers \(BBDB\).")

(defcustom riece-xfaceb-channels nil
  "*If non-nil, faces are only added in channels in this list.

You really want to set this to a list of small-ish channels that
you're in because having it set globally can slow Emacs to a crawl,
rendering it unusable if you're in some busy channels.

`riece-xfaceb-addremove-channel' can be used to interactively add or
remove the current channel to/from this list."
  :type '(repeat string)
  :group 'riece-looks)

(defcustom riece-xfaceb-prefer-cface-to-xface (featurep 'png)
  "*When non-nil, display colour face images instead of X-Face."
  :type 'boolean
  :group 'riece-looks)

(defun riece-xfaceb-addremove-channel (&optional remove)
  "*Add the current channel to `riece-xfaceb-channels'.

With optional argument, REMOVE, remove the current channel from the
list."
  (interactive "P")
  (if (or current-prefix-arg remove)
      ;; Remove channel.
      (progn
	(setq riece-xfaceb-channels
	      (remove (riece-identity-prefix riece-current-channel)
		      riece-xfaceb-channels))
	(message "Channel: %s removed from riece-xfaceb channel list."
		 (riece-identity-prefix riece-current-channel)))
    ;; Add channel.
    (add-to-list 'riece-xfaceb-channels
		 (riece-identity-prefix riece-current-channel))
    (message "Channel: %s added to riece-xfaceb channel list."
	     (riece-identity-prefix riece-current-channel)))
  (riece-emit-signal 'user-list-changed riece-current-channel))

(defun riece-xfaceb-face-to-png (face)
  "Base64 decode a Face header into a PNG.
Returns a string."
  (with-temp-buffer
    (insert face)
    (base64-decode-region (point-min) (point-max))
    (buffer-string)))

(defun riece-xfaceb-add-glyph (type extent data)
  "Adds a cface or xface glyph to an extent.

TYPE is a symbol, either `cface', or `xface'.
EXTENT is the extent to add the glyph to.
DATA is the image data from BBDB."
  (cond
   ((eq type 'cface)
    (let ((glyph (riece-xfaceb-face-to-png data)))
      (set-extent-begin-glyph
       extent
       (make-glyph `([png :data ,glyph])))))
   ((eq type 'xface)
    (let ((glyph (concat "X-Face: " data)))
      (set-extent-begin-glyph
       extent
       (make-glyph `([xface :data ,glyph
			    :foreground "black"
			    :background "white"])))))
   (t nil)))

(defun riece-xfaceb-update-user-list-buffer ()
  "Add X-Face or Colour Face images to channel users' buffer."
  (when (and (get 'riece-xfaceb 'riece-addon-enabled)
	     (or (null riece-xfaceb-channels)
		 (member (riece-identity-prefix riece-current-channel)
			 riece-xfaceb-channels)))
    (let ((users (ignore-errors 
		   (riece-with-server-buffer
		       (riece-identity-server riece-current-channel)
		     (riece-channel-get-users (riece-identity-prefix
					       riece-current-channel))))))
      (while users
	(let* ((name (regexp-quote (caar users)))
	       (str (cons 'ircnick name))
	       (records (bbdb-search (bbdb-records) nil nil nil str nil))
	       cface xface)
	  (mapcar
	   #'(lambda (record)
	       (setq xface (bbdb-record-getprop record 'face))
	       (setq cface (bbdb-record-getprop record 'cface)))
	   records)
	  (when (or cface xface)
	    (with-current-buffer riece-user-list-buffer
	      (goto-char (point-min))
	      (re-search-forward (regexp-quote name) nil t)
	      (beginning-of-line)
	      (let ((ext (extent-at (point))))
		(cond
		 ((and cface
		       (or riece-xfaceb-prefer-cface-to-xface
			   (not xface)))
		  (riece-xfaceb-add-glyph 'cface ext cface))
		 (xface (riece-xfaceb-add-glyph 'xface ext xface))
		 (t nil))))))
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
  (add-to-list 'riece-saved-forms 'riece-xfaceb-channels)
  (define-key riece-command-mode-map "\C-c\C-cx"
    #'riece-xfaceb-addremove-channel)
  (if riece-current-channel
      (riece-emit-signal 'user-list-changed riece-current-channel)))

(defun riece-xfaceb-disable ()
  (setq riece-saved-forms
	(remove 'riece-xfaceb-channels riece-saved-forms))
  (define-key riece-command-mode-map "\C-c\C-cx" nil)
  (if riece-current-channel
      (riece-emit-signal 'user-list-changed riece-current-channel)))

(provide 'riece-xfaceb)

;;; riece-xfaceb.el ends here

