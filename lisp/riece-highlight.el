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
  "Highlight your IRC buffer"
  :tag "Highlight"
  :prefix "riece-"
  :group 'riece)

(defgroup riece-highlight-faces nil
  "Faces for highlight your IRC buffer"
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
     (:foreground "PaleTurquoise" :underline t))
    (((class color)
      (background light))
     (:foreground "ForestGreen" :underline t))
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

(defun riece-dialogue-schedule-turn-on-font-lock ()
  (add-hook 'riece-channel-mode-hook
	    'riece-dialogue-turn-on-font-lock)
  (add-hook 'riece-others-mode-hook
	    'riece-dialogue-turn-on-font-lock)
  (add-hook 'riece-dialogue-mode-hook
	    'riece-dialogue-turn-on-font-lock))

(defun riece-channel-list-schedule-turn-on-font-lock ()
  (add-hook 'riece-channel-list-mode-hook
	    'riece-channel-list-turn-on-font-lock))

(defvar font-lock-support-mode)
(defun riece-dialogue-turn-on-font-lock ()
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(riece-dialogue-font-lock-keywords t))
  (make-local-variable 'font-lock-verbose)
  (setq font-lock-verbose nil)
  (when (boundp 'font-lock-support-mode)
    (make-local-variable 'font-lock-support-mode)
    (setq font-lock-support-mode nil))
  (make-local-hook 'font-lock-mode-hook)
  (setq font-lock-mode-hook nil)
  (turn-on-font-lock)
  (make-local-hook 'after-change-functions)
  (add-hook 'after-change-functions
	    'riece-dialogue-hide-prefix nil 'local))

(defun riece-dialogue-hide-prefix (start end length)
  (save-excursion
    (goto-char start)
    (if (looking-at riece-prefix-regexp)
	(put-text-property (match-beginning 1) (match-end 1) 'invisible t))))

(defun riece-channel-list-mark-current-channel (last)
  (if (and riece-channel-list-buffer-mode
	   riece-current-channel)
      (save-excursion
	(set-buffer riece-channel-list-buffer)
	(let ((inhibit-read-only t)
	      buffer-read-only)
	  (goto-char (point-min))
	  (if (re-search-forward "^\\( ?[0-9]+:\\)\\*" nil t)
	      (replace-match "\\1 "))
	  (goto-char (point-min))
	  (if (re-search-forward
	       (concat
		"^\\( ?[0-9]+:\\).\\("
		(regexp-quote (riece-format-identity riece-current-channel))
		"\\)$") nil t)
	      (replace-match "\\1*\\2"))))))

(defun riece-channel-list-turn-on-font-lock ()
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(riece-channel-list-font-lock-keywords t))
  (make-local-variable 'font-lock-verbose)
  (setq font-lock-verbose nil)
  (when (boundp 'font-lock-support-mode)
    (make-local-variable 'font-lock-support-mode)
    (setq font-lock-support-mode nil))
  (make-local-hook 'font-lock-mode-hook)
  (setq font-lock-mode-hook nil)
  (turn-on-font-lock))

(defun riece-highlight-insinuate ()
  (put 'riece-channel-mode 'font-lock-defaults
       '(riece-dialogue-font-lock-keywords t))
  (put 'riece-others-mode 'font-lock-defaults
       '(riece-dialogue-font-lock-keywords t))
  (put 'riece-dialogue-mode 'font-lock-defaults
       '(riece-dialogue-font-lock-keywords t))
  (add-hook 'riece-after-load-startup-hook
	    'riece-dialogue-schedule-turn-on-font-lock)
  (put 'riece-channel-list-mode 'font-lock-defaults
       '(riece-channel-list-font-lock-keywords t))
  (add-hook 'riece-after-switch-to-channel-functions
	    'riece-channel-list-mark-current-channel)
  (add-hook 'riece-after-load-startup-hook
	    'riece-channel-list-schedule-turn-on-font-lock))

(provide 'riece-highlight)

;;; riece-highlight.el ends here
