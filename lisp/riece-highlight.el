;;; riece-highlight.el --- coloring IRC buffers
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

(defcustom riece-change-face 'riece-change-face
  "Face used for displaying \"*** Change:\" line."
  :type 'face
  :group 'riece-highlight-faces)

(defcustom riece-notice-face 'riece-notice-face
  "Face used for displaying \"*** Notice:\" line."
  :type 'face
  :group 'riece-highlight-faces)

(defcustom riece-wallops-face 'riece-wallops-face
  "Face used for displaying \"*** Wallops:\" line."
  :type 'face
  :group 'riece-highlight-faces)
  
(defcustom riece-error-face 'riece-error-face
  "Face used for displaying \"*** Error:\" line."
  :type 'face
  :group 'riece-highlight-faces)

(defcustom riece-info-face 'riece-info-face
  "Face used for displaying \"*** Info:\" line."
  :type 'face
  :group 'riece-highlight-faces)

(defcustom riece-server-face 'riece-server-face
  "Face used for displaying \"(from server)\" extent."
  :type 'face
  :group 'riece-highlight-faces)

(defcustom riece-prefix-face 'riece-prefix-face
  "Face used for displaying \"<nick>\" extent."
  :type 'face
  :group 'riece-highlight-faces)

(defface riece-change-face
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

(defface riece-notice-face
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

(defface riece-wallops-face
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

(defface riece-error-face
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

(defface riece-info-face
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

(defface riece-server-face
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

(defface riece-prefix-face
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

(defcustom riece-highlight-font-lock-keywords
  (append
   (list (list (concat "^" riece-time-prefix-regexp
		       "\\(<[^>]+>\\|>[^<]+<\\|([^)]+)\\|{[^}]+}\\|=[^=]+=\\)")
	       '(1 riece-prefix-face append t)))
   ;; set property to the whole line
   (mapcar
    (lambda (line)
      (cons
       (concat
	"^" riece-time-prefix-regexp "\\("
	(regexp-quote
	 (symbol-value (intern (format "riece-%s-prefix" line))))
	".*\\)$")
       (list 1 (intern (format "riece-%s-face" line)) t t)))
    '(change notice wallops error info))
   (list (list "(from [^)]+)$" 0 riece-server-face t)))
  "Normal and deformed faces for IRC normal line."
  :type '(repeat (list string))
  :group 'riece-highlight)

(defun riece-highlight-schedule-turn-on-font-lock ()
  (add-hook 'riece-channel-mode-hook
	    'riece-highlight-turn-on-font-lock)
  (add-hook 'riece-others-mode-hook
	    'riece-highlight-turn-on-font-lock)
  (add-hook 'riece-dialogue-mode-hook
	    'riece-highlight-turn-on-font-lock))

(defvar font-lock-support-mode)
(defun riece-highlight-turn-on-font-lock ()
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(riece-highlight-font-lock-keywords t))
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
	    'riece-highlight-hide-prefix nil 'local))

(defun riece-highlight-hide-prefix (start end length)
  (save-excursion
    (goto-char start)
    (if (looking-at riece-prefix-regexp)
	(put-text-property (match-beginning 1) (match-end 1) 'invisible t))))

(defun riece-highlight-insinuate ()
  (put 'riece-channel-mode 'font-lock-defaults
       '(riece-highlight-font-lock-keywords t))
  (put 'riece-others-mode 'font-lock-defaults
       '(riece-highlight-font-lock-keywords t))
  (put 'riece-dialogue-mode 'font-lock-defaults
       '(riece-highlight-font-lock-keywords t))
  (add-hook 'riece-after-load-startup-hook
	    'riece-highlight-schedule-turn-on-font-lock))

(provide 'riece-highlight)

;;; riece-highlight.el ends here
