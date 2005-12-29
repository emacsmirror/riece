;;; riece-ctlseq.el --- mark up control sequences in IRC buffers
;; Copyright (C) 1998-2004 Daiki Ueno

;; Author: Daiki Ueno <ueno@unixuser.org>
;; Created: 1998-09-28
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

;; NOTE: This is an add-on module for Riece.

;;; Code:

(require 'riece-message)
(require 'riece-misc)

(defgroup riece-ctlseq nil
  "Mark up control sequences in IRC buffer."
  :prefix "riece-"
  :group 'riece)

(defcustom riece-ctlseq-colors
  '("white" "black" "red" "orange" "yellow" "LightGreen" "DarkOliveGreen"
    "cyan4" "turquoise" "blue" "black" "black" "black" "black" "black"
    "DarkBlue" "purple1" "purple2" "purple3" "magenta")
  "List of colors can be used with ^C<fg>,<bg>."
  :group 'riece-ctlseq
  :type '(repeat (string :tag "Color")))

(defcustom riece-ctlseq-hide-controls t
  "If non-nil, control characters are hidden."
  :group 'riece-ctlseq
  :type 'boolean)

(defcustom riece-ctlseq-face-cache-size 128
  "Maximum length of the internal face cache."
  :group 'riece-ctlseq
  :type 'integer)

(defvar riece-ctlseq-face-cache nil)
(defvar riece-ctlseq-face-counter 0)

(defconst riece-ctlseq-description
  "Mark up control sequences in IRC buffers.")

(defun riece-ctlseq-compatible-attributes-p (this other)
  (let ((pointer this))
    (catch 'mismatched
      (while pointer
	(unless (equal (plist-get other (car pointer)) (nth 1 pointer))
	  (throw 'mismatched nil))
	(setq pointer (nthcdr 2 pointer)))
      t)))

(defun riece-ctlseq-face-foreground-name (face)
  "Return the name of FACE's foreground color."
  (if (fboundp 'face-foreground-name)	;XEmacs
      (face-foreground-name face)
    (face-foreground face)))

(defun riece-ctlseq-face-background-name (face)
  "Return the name of FACE's background color."
  (if (fboundp 'face-background-name)	;XEmacs
      (face-background-name face)
    (face-background face)))

(defun riece-ctlseq-make-face (attrs)
  (let* ((face-name (intern (format "riece-ctlseq-face-%d"
				    (prog1 riece-ctlseq-face-counter
				      (setq riece-ctlseq-face-counter
					    (1+ riece-ctlseq-face-counter))))))
	 (face (make-face face-name))
	 foreground
	 background)
    (if (plist-get attrs 'bold)
	(make-face-bold face))
    (if (plist-get attrs 'underline)
	(set-face-underline-p face t))
    (if (setq foreground (plist-get attrs 'foreground))
	(set-face-foreground face foreground))
    (if (setq background (plist-get attrs 'background))
	(set-face-background face background))
    (when (plist-get attrs 'inverse-video)
      (setq foreground (or (riece-ctlseq-face-background-name face)
			   (riece-ctlseq-face-background-name 'default))
	    background (or (riece-ctlseq-face-foreground-name face)
			   (riece-ctlseq-face-foreground-name 'default)))
      (set-face-foreground face foreground)
      (set-face-background face background))
    (put face-name 'riece-ctlseq-attributes attrs)
    face-name))

(defun riece-ctlseq-face-from-cache (attrs)
  (if (null attrs)
      'default
    (let ((pointer riece-ctlseq-face-cache)
	  last-pointer
	  other)
      (catch 'found
	(while pointer
	  (setq other (get (car pointer) 'riece-ctlseq-attributes))
	  (when (and (riece-ctlseq-compatible-attributes-p attrs other)
		     (riece-ctlseq-compatible-attributes-p other attrs))
	    (if last-pointer
		(setcdr last-pointer (cdr pointer)))
	    (throw 'found (setcar riece-ctlseq-face-cache (car pointer))))
	  (setq last-pointer pointer
		pointer (cdr pointer)))
	(if (>= (length riece-ctlseq-face-cache)
		riece-ctlseq-face-cache-size)
	    (setq riece-ctlseq-face-cache
		  (butlast riece-ctlseq-face-cache)))
	(setq riece-ctlseq-face-cache
	      (cons (riece-ctlseq-make-face attrs)
		    riece-ctlseq-face-cache))
	(car riece-ctlseq-face-cache)))))

(defun riece-ctlseq-update-attributes (tag attrs)
  (cond
   ((eq (aref tag 0) ?\x2)		;^B
    (plist-put attrs 'bold (not (plist-get attrs 'bold))))
   ((eq (aref tag 0) ?\xF))		;^O
   ((eq (aref tag 0) ?\x16)		;^V
    (plist-put attrs 'inverse-video (not (plist-get attrs 'inverse-video))))
   ((eq (aref tag 0) ?\x1F)		;^_
    (plist-put attrs 'underline (not (plist-get attrs 'underline))))
   ((string-match "\x3\\([0-9]+\\)?\\(,[0-9]+\\)?" tag)	;^C<fg>,<bg>
    (if (match-beginning 1)
	(setq attrs (plist-put attrs 'foreground
			       (nth (string-to-number (match-string 1 tag))
				    riece-ctlseq-colors))))
    (if (match-beginning 2)
	(setq attrs (plist-put attrs 'background
			       (nth (string-to-number
				     (substring (match-string 2 tag) 1))
				    riece-ctlseq-colors))))
    attrs)))

(defun riece-ctlseq-message-filter (message)
  (if (get 'riece-ctlseq 'riece-addon-enabled)
      (let ((start 0)
	    (end (length (riece-message-text message)))
	    attrs)
	(while (string-match
		"[\x2\xF\x16\x1F]\\|\x3\\([0-9]+\\)?\\(,[0-9]+\\)?"
		(riece-message-text message) start)
	  (if riece-ctlseq-hide-controls
	      (put-text-property (match-beginning 0) (match-end 0)
				 'invisible 'riece-ctlseq
				 (riece-message-text message)))
	  (if attrs
	      (put-text-property start (match-beginning 0)
				 'riece-ctlseq-attributes (copy-sequence attrs)
				 (riece-message-text message)))
	  (setq start (match-end 0)
		attrs (riece-ctlseq-update-attributes
		       (match-string 0 (riece-message-text message)) attrs)))
	(if (and (< start end) attrs)
	    (put-text-property start end
			       'riece-overlay-face
			       (riece-ctlseq-face-from-cache attrs)
			       (riece-message-text message)))))
  message)

(defun riece-ctlseq-requires ()
  '(riece-highlight))

(defun riece-ctlseq-insinuate ()
  (add-hook 'riece-message-filter-functions 'riece-ctlseq-message-filter))

(defun riece-ctlseq-uninstall ()
  (remove-hook 'riece-message-filter-functions 'riece-ctlseq-message-filter))

(provide 'riece-ctlseq)

;;; riece-ctlseq.el ends here
