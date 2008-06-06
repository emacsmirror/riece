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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; NOTE: This is an add-on module for Riece.

;;; Code:

(require 'riece-globals)
(require 'riece-options)		;riece-channel-list-buffer-mode
(require 'riece-identity)		;riece-format-identity
(require 'riece-misc)
(require 'font-lock)
(require 'derived)

(defgroup riece-highlight nil
  "Decorate IRC buffers with faces and fonts."
  :tag "Highlight"
  :prefix "riece-"
  :group 'riece)

(defgroup riece-highlight-faces nil
  "Faces for highlight IRC buffers."
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
   '((riece-highlight-server-match 0 riece-dialogue-server-face t)))
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
  :type '(repeat (cons character symbol))
  :group 'riece-highlight)

(defcustom riece-channel-list-font-lock-keywords
  '(("^[ 0-9][0-9]:\\(.\\)\\(.*\\)"
     (2 (or (cdr (assq (aref (match-string 1) 0)
		       riece-channel-list-mark-face-alist))
	    riece-channel-list-default-face))))
  "Default expressions to highlight in riece-channel-list-mode."
  :type '(repeat (list string))
  :group 'riece-highlight)

(unless (riece-facep 'riece-modeline-current-face)
  (make-face 'riece-modeline-current-face
	     "Face used for displaying the current channel in modeline.")
  (if (featurep 'xemacs)
      (set-face-parent 'riece-modeline-current-face 'modeline))
  (set-face-foreground 'riece-modeline-current-face
		       (face-foreground 'riece-channel-list-current-face)))

(defconst riece-highlight-description
  "Highlight IRC buffers.")

(defun riece-highlight-server-match (limit)
  (and (re-search-forward "(from [^)]+)$" limit t)
       (get-text-property (match-beginning 0) 'riece-server-name)))

(defun riece-highlight-setup-dialogue ()
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(riece-dialogue-font-lock-keywords t))
  ;; In XEmacs, auto-initialization of font-lock is not affective
  ;; when buffer-file-name is not set.
  (font-lock-set-defaults)
  (riece-make-local-hook 'after-change-functions)
  (add-hook 'after-change-functions
	    'riece-highlight-hide-prefix nil t)
  (if (get 'riece-highlight 'riece-addon-enabled)
      (font-lock-mode 1)))

(defun riece-highlight-setup-channel-list ()
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(riece-channel-list-font-lock-keywords t))
  ;; In XEmacs, auto-initialization of font-lock is not affective
  ;; when buffer-file-name is not set.
  (font-lock-set-defaults)
  (if (get 'riece-highlight 'riece-addon-enabled)
      (font-lock-mode 1)))

(defun riece-highlight-hide-prefix (start end length)
  (save-excursion
    (goto-char start)
    (if (looking-at riece-prefix-regexp)
	(put-text-property (match-beginning 1) (match-end 1) 'invisible t))))

(defun riece-highlight-put-overlay-faces (start end)
  (if (get 'riece-highlight 'riece-addon-enabled)
      (riece-scan-property-region
       'riece-overlay-face
       start end
       (lambda (start end)
	 (riece-overlay-put (riece-make-overlay start end)
			    'face
			    (get-text-property start 'riece-overlay-face))))))

(defun riece-highlight-format-identity-for-channel-list-indicator (index
								   identity)
  (if (and (get 'riece-highlight 'riece-addon-enabled)
	   (riece-identity-equal identity riece-current-channel))
      (let ((string (riece-format-identity identity))
	    (start 0))
	;; Escape % -> %%.
	(while (string-match "%" string start)
	  (setq start (1+ (match-end 0))
		string (replace-match "%%" nil nil string)))
	(list (format "%d:" index)
	      (riece-propertize-modeline-string
	       string 'face 'riece-modeline-current-face)))))

(defun riece-highlight-insinuate ()
  (put 'riece-channel-mode 'font-lock-defaults
       '(riece-dialogue-font-lock-keywords t))
  (put 'riece-others-mode 'font-lock-defaults
       '(riece-dialogue-font-lock-keywords t))
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

(defun riece-highlight-uninstall ()
  (let ((buffers riece-buffer-list))
    (save-excursion
      (while buffers
	(set-buffer (car buffers))
	(if (riece-derived-mode-p 'riece-dialogue-mode)
	    (remove-hook 'after-change-functions
			 'riece-highlight-hide-prefix t))
	(setq buffers (cdr buffers)))))
  (riece-remprop 'riece-channel-mode 'font-lock-defaults)
  (riece-remprop 'riece-others-mode 'font-lock-defaults)
  (riece-remprop 'riece-dialogue-mode 'font-lock-defaults)
  (remove-hook 'riece-dialogue-mode-hook
	       'riece-highlight-setup-dialogue)
  (riece-remprop 'riece-channel-list-mode 'font-lock-defaults)
  (remove-hook 'riece-channel-list-mode-hook
	       'riece-highlight-setup-channel-list)
  (remove-hook 'riece-format-identity-for-channel-list-indicator-functions
	       'riece-highlight-format-identity-for-channel-list-indicator)
  (remove-hook 'riece-after-insert-functions
	       'riece-highlight-put-overlay-faces))

(defun riece-highlight-enable ()
  (let ((buffers riece-buffer-list))
    (while buffers
      (if (with-current-buffer (car buffers)
	    (riece-derived-mode-p 'riece-dialogue-mode
				  'riece-channel-list-mode))
	  (with-current-buffer (car buffers)
	    (font-lock-mode 1)))
      (setq buffers (cdr buffers)))))

(defun riece-highlight-disable ()
  (let ((buffers riece-buffer-list))
    (while buffers
      (if (with-current-buffer (car buffers)
	    (riece-derived-mode-p 'riece-dialogue-mode
				  'riece-channel-list-mode))
	  (with-current-buffer (car buffers)
	    (font-lock-mode 0)))
      (setq buffers (cdr buffers)))))

(provide 'riece-highlight)

;;; riece-highlight.el ends here
