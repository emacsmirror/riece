;;; riece-highlight.el --- highlight IRC buffers
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

;;; Code:

(require 'riece-globals)
(require 'riece-options)		;riece-channel-list-buffer-mode
(require 'riece-identity)		;riece-format-identity
(require 'font-lock)

(defgroup riece-highlight nil
  "Highlight IRC buffers"
  :tag "Highlight"
  :prefix "riece-"
  :group 'riece)

(defgroup riece-highlight-faces nil
  "Faces for highlight IRC buffers"
  :tag "Faces"
  :prefix "riece-highlight-"
  :group 'riece-highlight)

(defface riece-dialogue-change-face
  '((((class color)
      (background dark))
     (:foreground "cyan" :bold t))
    (((class color)
      (background light))
     (:foreground "RoyalBlue" :bold t))
    (t
     (:bold t)))
  "Face used for displaying \"*** Change:\" line"
  :group 'riece-highlight-faces)
(defvar riece-dialogue-change-face 'riece-dialogue-change-face)

(defface riece-dialogue-notice-face
  '((((class color)
      (background dark))
     (:foreground "green2" :bold t))
    (((class color)
      (background light))
     (:foreground "MidnightBlue" :bold t))
    (t
     (:bold t)))
  "Face used for displaying \"*** Notice:\" line"
  :group 'riece-highlight-faces)
(defvar riece-dialogue-notice-face 'riece-dialogue-notice-face)

(defface riece-dialogue-wallops-face
  '((((class color)
      (background dark))
     (:foreground "yellow" :bold t))
    (((class color)
      (background light))
     (:foreground "blue4" :bold t))
    (t
     (:bold t)))
  "Face used for displaying \"*** Wallops:\" line"
  :group 'riece-highlight-faces)
(defvar riece-dialogue-wallops-face 'riece-dialogue-wallops-face)

(defface riece-dialogue-error-face
  '((((class color)
      (background dark))
     (:foreground "cornflower blue" :bold t))
    (((class color)
      (background light))
     (:foreground "DarkGreen"))
    (t
     (:bold t)))
  "Face used for displaying \"*** Error:\" line"
  :group 'riece-highlight-faces)
(defvar riece-dialogue-error-face 'riece-dialogue-error-face)

(defface riece-dialogue-info-face
  '((((class color)
      (background dark))
     (:foreground "PaleTurquoise" :bold t))
    (((class color)
      (background light))
     (:foreground "RoyalBlue"))
    (t
     (:bold t)))
  "Face used for displaying \"*** Info:\" line"
  :group 'riece-highlight-faces)
(defvar riece-dialogue-info-face 'riece-dialogue-info-face)

(defface riece-dialogue-server-face
  '((((class color)
      (background dark))
     (:foreground "Gray70"))
    (((class color)
      (background light))
     (:foreground "DimGray"))
    (t
     (:bold t)))
  "Face used for displaying \"(from server)\" extent."
  :group 'riece-highlight-faces)
(defvar riece-dialogue-server-face 'riece-dialogue-server-face)

(defface riece-dialogue-prefix-face
  '((((class color)
      (background dark))
     (:foreground "moccasin"))
    (((class color)
      (background light))
     (:foreground "firebrick"))
    (t
     (:bold nil)))
  "Face used for displaying \"<nick>\" extent"
  :group 'riece-highlight-faces)
(defvar riece-dialogue-prefix-face 'riece-dialogue-prefix-face)

(defcustom riece-dialogue-font-lock-keywords
  (append
   (list (list (concat "^" riece-time-prefix-regexp
		       "\\(<[^>]+>\\|>[^<]+<\\|([^)]+)\\|{[^}]+}\\|=[^=]+=\\)")
	       '(1 riece-dialogue-prefix-face append t)))
   ;; set property to the whole line
   (mapcar
    (lambda (line)
      (cons
       (concat
	"^" riece-time-prefix-regexp "\\("
	(regexp-quote
	 (symbol-value (intern (format "riece-%s-prefix" line))))
	".*\\)$")
       (list 1 (intern (format "riece-dialogue-%s-face" line)) t t)))
    '(change notice wallops error info))
   (list (list "(from [^)]+)$" 0 riece-dialogue-server-face t)))
  "Default expressions to highlight in riece-dialogue-mode."
  :type '(repeat (list string))
  :group 'riece-highlight)

(defface riece-channel-list-default-face
  '((t ()))
  "Face used for displaying channels."
  :group 'riece-highlight-faces)
(defvar riece-channel-list-default-face 'riece-channel-list-default-face)

(defface riece-channel-list-current-face
  '((((class color)
      (background dark))
     (:foreground "turquoise" :underline t))
    (((class color)
      (background light))
     (:foreground "SeaGreen" :underline t))
    (t
     ()))
  "Face used for displaying the current channel."
  :group 'riece-highlight-faces)
(defvar riece-channel-list-current-face 'riece-channel-list-current-face)

(defcustom riece-channel-list-mark-face-alist
  '((?* . riece-channel-list-current-face))
  "An alist mapping marks on riece-channel-list-buffer to faces."
  :type 'list
  :group 'riece-highlight)

(defcustom riece-channel-list-font-lock-keywords
  '(("^[ 0-9][0-9]:\\(.\\)\\(.*\\)"
     (2 (or (cdr (assq (aref (match-string 1) 0)
		       riece-channel-list-mark-face-alist))
	    riece-channel-list-default-face))))
  "Default expressions to highlight in riece-channel-list-mode."
  :type '(repeat (list string))
  :group 'riece-highlight)

(defvar riece-highlight-enabled nil)

(defconst riece-highlight-description
  "Highlight IRC buffers")

(defun riece-highlight-setup-dialogue ()
  (if (featurep 'xemacs)
      ;; In XEmacs, auto-initialization of font-lock is not affective
      ;; when buffer-file-name is not set.
      (font-lock-set-defaults)
    (make-local-variable 'font-lock-defaults)
    (setq font-lock-defaults '(riece-dialogue-font-lock-keywords t)))
  (make-local-hook 'after-change-functions)
  (add-hook 'after-change-functions
	    'riece-highlight-hide-prefix nil 'local))

(defun riece-highlight-setup-channel-list ()
  (if (featurep 'xemacs)
      ;; In XEmacs, auto-initialization of font-lock is not affective
      ;; when buffer-file-name is not set.
      (font-lock-set-defaults)
    (make-local-variable 'font-lock-defaults)
    (setq font-lock-defaults '(riece-channel-list-font-lock-keywords t))))

(defun riece-highlight-hide-prefix (start end length)
  (save-excursion
    (goto-char start)
    (if (looking-at riece-prefix-regexp)
	(put-text-property (match-beginning 1) (match-end 1) 'invisible t))))

(defun riece-highlight-put-overlay-faces (start end)
  (if riece-highlight-enabled
      (riece-scan-property-region
       'riece-overlay-face
       start end
       (lambda (start end)
	 (riece-overlay-put (riece-make-overlay start end)
			    'face
			    (get-text-property start 'riece-overlay-face))))))

(defun riece-highlight-format-identity-for-channel-list-indicator (index
								   identity)
  (if (and riece-highlight-enabled
	   (riece-identity-equal identity riece-current-channel))
      (let ((string (riece-format-identity identity))
	    (start 0))
	;; Escape % -> %%.
	(while (string-match "%" string start)
	  (setq start (1+ (match-end 0))
		string (replace-match "%%" nil nil string)))
	(list (format "%d:" index)
	      (riece-propertize-modeline-string
	       string 'face 'riece-channel-list-current-face)))))

(defun riece-highlight-insinuate ()
  (put 'riece-channel-mode 'font-lock-defaults
       '(riece-dialogue-font-lock-keywords t))
  (add-hook 'riece-channel-mode-hook
	    'riece-highlight-setup-dialogue)
  (put 'riece-others-mode 'font-lock-defaults
       '(riece-dialogue-font-lock-keywords t))
  (add-hook 'riece-others-mode-hook
	    'riece-highlight-setup-dialogue)
  (put 'riece-dialogue-mode 'font-lock-defaults
       '(riece-dialogue-font-lock-keywords t))
  (add-hook 'riece-dialogue-mode-hook
	    'riece-highlight-setup-dialogue)
  (put 'riece-channel-list-mode 'font-lock-defaults
       '(riece-channel-list-font-lock-keywords t))
  (add-hook 'riece-channel-list-mode-hook
	    'riece-highlight-setup-channel-list)
  (add-hook 'riece-format-identity-for-channel-list-indicator-functions
	    'riece-highlight-format-identity-for-channel-list-indicator)
  (add-hook 'riece-after-insert-functions
	    'riece-highlight-put-overlay-faces))

(defun riece-highlight-enable ()
  (let ((buffers riece-buffer-list))
    (while buffers
      (if (memq (derived-mode-class
	       (with-current-buffer (car buffers)
		 major-mode))
		'(riece-dialogue-mode riece-channel-list-mode))
	  (with-current-buffer (car buffers)
	    (font-lock-mode 1)))
      (setq buffers (cdr buffers))))
  (setq riece-highlight-enabled t))

(defun riece-highlight-disable ()
  (let ((buffers riece-buffer-list))
    (while buffers
      (if (memq (derived-mode-class
	       (with-current-buffer (car buffers)
		 major-mode))
		'(riece-dialogue-mode riece-channel-list-mode))
	  (with-current-buffer (car buffers)
	    (font-lock-mode -1)))
      (setq buffers (cdr buffers))))
  (setq riece-highlight-enabled nil))

(provide 'riece-highlight)

;;; riece-highlight.el ends here
