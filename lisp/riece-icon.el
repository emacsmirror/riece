;;; riece-icon.el --- iconify buffer strings
;; Copyright (C) 1'center8-2003 Daiki Ueno

;; Author: Daiki Ueno <ueno@unixuser.org>
;; Created: 1'center8-09-28
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
;; (add-to-list 'riece-addons 'riece-icon)

;;; Code:

(defvar riece-channel-list-icons
  '((" " . "/* XPM */
static char * blank_xpm[] = {
\"12 12 1 1\",
\" 	c None\",
\"            \",
\"            \",
\"            \",
\"            \",
\"            \",
\"            \",
\"            \",
\"            \",
\"            \",
\"            \",
\"            \",
\"            \"};")
    ("!" . "/* XPM */
static char * balloon_xpm[] = {
\"12 12 3 1\",
\"       c None\",
\"+      c #FFDD99\",
\"@      c #000000\",
\"            \",
\"    ++++    \",
\"  ++++++++  \",
\" ++@@@@@@++ \",
\" ++++++++++ \",
\" ++@@@@@@++ \",
\" ++++++++++ \",
\" ++@@@@@@++ \",
\"  ++++++++  \",
\"   ++++++   \",
\"   +++      \",
\"   +        \"};")
    ("+" . "/* XPM */
static char * check_xpm[] = {
\"12 12 3 1\",
\" 	c None\",
\".	c #9696FF\",
\"+	c #5959FF\",
\"            \",
\"            \",
\" ..      .. \",
\".++.    .++.\",
\" .++.  .++. \",
\"  .++..++.  \",
\"   .++++.   \",
\"    .++.    \",
\"     ..     \",
\"            \",
\"            \",
\"            \"};")
    ("*" . "/* XPM */
static char * active_xpm[] = {
\"12 12 3 1\",
\" 	c None\",
\".	c #96FF96\",
\"+	c #59FF59\",
\"            \",
\"     ..     \",
\"     .+.    \",
\" .....++.   \",
\" .+++++++.  \",
\" .++++++++. \",
\" .+++++++.  \",
\" .....++.   \",
\"     .+.    \",
\"     ..     \",
\"            \",
\"            \"};")))

(defvar riece-user-list-icons
  '((" " . "/* XPM */
static char * blank_xpm[] = {
\"12 12 1 1\",
\" 	c None\",
\"            \",
\"            \",
\"            \",
\"            \",
\"            \",
\"            \",
\"            \",
\"            \",
\"            \",
\"            \",
\"            \",
\"            \"};")
    ("@" . "/* XPM */
static char * spiral_xpm[] = {
\"12 12 3 1\",
\" 	c None\",
\".	c #FF5959\",
\"+	c #FF9696\",
\"            \",
\"            \",
\"    +++++   \",
\"   ++...++  \",
\"  ++.+++.++ \",
\"  +.++.++.+ \",
\"  +.+.+.+.+ \",
\"  +.+.+++.+ \",
\"  +.++...++ \",
\"  ++.+++++.+\",
\"   ++.....+ \",
\"    ++++++  \"};")
    ("+" . "/* XPM */
static char * cross_xpm[] = {
\"12 12 3 1\",
\" 	c None\",
\".	c #7F7F7F\",
\"+	c #B2B2B2\",
\"     ++     \",
\"    +..+    \",
\"    +..+    \",
\"  +++..+++  \",
\" +........+ \",
\" +........+ \",
\"  +++..+++  \",
\"    +..+    \",
\"    +..+    \",
\"    +..+    \",
\"    +..+    \",
\"     ++     \"};")))

(defvar riece-pointer-icon
  "/* XPM */
static char * a_xpm[] = {
\"14 14 5 1\",
\" 	c None\",
\".	c #FF9646\",
\"+	c #FF5909\",
\"@	c #FF7020\",
\"*	c #FFA500\",
\"              \",
\"  @@@@@@@@@@@ \",
\" @*.++++++.**@\",
\" @*.++...++.*@\",
\" @*.++.*.++.*@\",
\" @*.++...+.**@\",
\" @*.+++.+.***@\",
\" @*.++.*.+.**@\",
\" @*.++.*.++.*@\",
\" @*.++.*.++.*@\",
\" @*.++.*.++.*@\",
\" @**..***..**@\",
\"  @@@@@@@@@@@ \",
\"              \"};")

(defun riece-icon-available-p ()
  (if (featurep 'xemacs)
      (featurep 'xpm)
    (if (fboundp 'image-type-available-p)
	(image-type-available-p 'xpm))))

(eval-and-compile
  (if (featurep 'xemacs)
      (defun riece-icon-make-image (data string)
	(make-glyph (list (vector 'xpm :data data)
			  (vector 'string :data string))))
    (defun riece-icon-make-image (data string)
      (create-image data 'xpm t :ascent 'center))))

(defun riece-icon-make-images (alist)
  (let ((pointer (setq alist (copy-alist alist))))
    (while pointer
      (setcdr (car pointer)
	      (riece-icon-make-image (cdr (car pointer)) (car (car pointer))))
      (setq pointer (cdr pointer)))
    alist))

(eval-and-compile
  (if (featurep 'xemacs)
      (defun riece-icon-add-image-region (image start end)
	(map-extents
	 (lambda (extent ignore)
	   (if (or (extent-property extent 'riece-icon-user-list-extent)
		   (extent-property extent 'riece-icon-user-list-annotation))
	       (delete-extent extent)))
	 (current-buffer) start end)
	(let ((extent (make-extent start end))
	      (annotation (make-annotation image end 'text)))
	  (set-extent-property extent 'end-open t)
	  (set-extent-property extent 'start-open t)
	  (set-extent-property extent 'invisible t)
	  (set-extent-property extent 'intangible t)
	  (set-extent-property annotation
			       'riece-icon-user-list-extent extent)
	  (set-extent-property extent
			       'riece-icon-user-list-annotation annotation)))
    (defun riece-icon-add-image-region (image start end)
      (let ((inhibit-read-only t)
	    buffer-read-only)
	(add-text-properties start end
			     (list 'display
				   image
				   'rear-nonsticky (list 'display)))))))

(defun riece-icon-update-user-list-buffer ()
  (let ((images (riece-icon-make-images riece-user-list-icons)))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^[ @+]" nil t)
	(riece-icon-add-image-region
	 (cdr (assoc (match-string 0) images))
	 (1- (point)) (point))))))

(defun riece-icon-update-channel-list-buffer ()
  (let ((images (riece-icon-make-images riece-channel-list-icons)))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^ ?[0-9]+:\\([ !+*]\\)" nil t)
	(riece-icon-add-image-region
	 (cdr (assoc (match-string 1) images))
	 (match-beginning 1) (match-end 1))))))

(eval-and-compile
  (if (featurep 'xemacs)
      (progn
	(defvar riece-icon-xemacs-modeline-left-extent
	  (copy-extent modeline-buffer-id-left-extent))

	(defvar riece-icon-xemacs-modeline-right-extent
	  (copy-extent modeline-buffer-id-right-extent))

	(defun riece-icon-modeline-buffer-identification (line)
	  "Decorate 1st element of `mode-line-buffer-identification' LINE.
Modify whole identification by side effect."
	  (let ((id (car line)) chopped)
	    (if (and (stringp id) (string-match "^Riece:" id))
		(progn
		  (setq chopped (substring id 0 (match-end 0))
			id (substring id (match-end 0)))
		  (nconc
		   (list
		    (let ((glyph
			   (make-glyph
			    (nconc
			     (if (featurep 'xpm)
				 (list (vector 'xpm :data
					       riece-pointer-icon)))
			     (list (vector 'string :data chopped))))))
		      (set-glyph-face glyph 'modeline-buffer-id)
		      (cons riece-icon-xemacs-modeline-left-extent glyph))
		    (cons riece-icon-xemacs-modeline-right-extent id))
		   (cdr line)))
	      line))))
    (condition-case nil
	(progn
	  (require 'image)
	  (defun riece-icon-modeline-buffer-identification (line)
	    "Decorate 1st element of `mode-line-buffer-identification' LINE.
Modify whole identification by side effect."
	    (let ((id (copy-sequence (car line)))
		  (image
		   (if (image-type-available-p 'xpm)
		       (create-image riece-pointer-icon 'xpm t
				     :ascent 'center))))
	      (when (and image
			 (stringp id) (string-match "^Riece:" id))
		(add-text-properties 0 (length id)
				     (list 'display image
					   'rear-nonsticky (list 'display))
				     id)
		(setcar line id))
	      line)))
      (error
       (defalias 'riece-icon-modeline-buffer-identification 'identity)))))

(defun riece-icon-insinuate ()
  (defalias 'riece-mode-line-buffer-identification
    'riece-icon-modeline-buffer-identification)
  (add-hook 'riece-user-list-mode-hook
	    (lambda ()
	      (if (riece-icon-available-p)
		  (add-hook 'riece-update-buffer-functions
			    'riece-icon-update-user-list-buffer t t))))
  (add-hook 'riece-channel-list-mode-hook
	    (lambda ()
	      (if (riece-icon-available-p)
		  (add-hook 'riece-update-buffer-functions
			    'riece-icon-update-channel-list-buffer t t)))))

(provide 'riece-icon)

;;; riece-icon.el ends here
