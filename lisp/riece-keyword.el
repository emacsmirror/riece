;;; riece-keyword.el --- highlight keywords in channel buffers
;; Copyright (C) 1998-2003 Daiki Ueno

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
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; To use, add the following line to your ~/.riece/init.el:
;; (add-to-list 'riece-addons 'riece-keyword)

;;; Code:

(defgroup riece-keyword nil
  "Highlight keyword in IRC buffer."
  :group 'riece-vars)

(defcustom riece-keywords nil
  "Keywords to be highlightened."
  :type '(repeat string)
  :group 'riece-keyword)

(defcustom riece-notify-keyword-functions nil
  "Functions used to notify keyword match."
  :type '(list function)
  :group 'riece-keyword)

(defface riece-keyword-face
  '((((class color))
     (:foreground "red" :underline t))
    (t
     ()))
  "Face used for highlightening matching keyword."
  :group 'riece-highlight-faces)
(defvar riece-keyword-face 'riece-keyword-face)

;;; The old XEmacs package doesn't have autoload setting for regexp-opt.
(autoload 'regexp-opt "regexp-opt")
(defun riece-keyword-message-filter (message)
  (if riece-keywords
      (let ((regexp (regexp-opt riece-keywords))
	    (index 0))
	(while (string-match regexp (riece-message-text message) index)
	  (if (memq 'riece-highlight riece-addons)
	      (put-text-property (match-beginning 0) (match-end 0)
				 'riece-keyword t
				 (riece-message-text message)))
	  (save-match-data
	    (run-hook-with-args 'riece-notify-keyword-functions
				(match-string 0 (riece-message-text message))))
	  (setq index (match-end 0)))))
  message)

(defun riece-keyword-map-region (start end function)
  (catch 'done
    (while t
      ;; Search for the beginning of the button region.
      (unless (get-text-property start 'riece-keyword)
	(setq start (next-single-property-change start 'riece-keyword
						 nil end)))
      (if (= start end)
	  (throw 'done nil))
      ;; Search for the end of the button region.
      (let ((button-end (next-single-property-change start 'riece-keyword
						     nil end)))
	(if (= button-end end)
	    (throw 'done nil))
	(funcall function start button-end)
	(setq start button-end)))))

(defun riece-keyword-scan-region (start end)
  (riece-keyword-map-region
   start end
   (lambda (start end)
     (riece-overlay-put (riece-make-overlay start end)
			'face riece-keyword-face))))

(defun riece-keyword-requires ()
  (if (memq 'riece-highlight riece-addons)
      '(riece-highlight)))

(defun riece-keyword-insinuate ()
  (add-hook 'riece-message-filter-functions 'riece-keyword-message-filter)
  (add-hook 'riece-after-insert-functions 'riece-keyword-scan-region))

(provide 'riece-keyword)

;;; riece-keyword.el ends here