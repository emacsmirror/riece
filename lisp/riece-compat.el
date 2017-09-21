;;; riece-compat.el --- compatibility functions -*- lexical-binding: t -*-
;; Copyright (C) 1998-2003 Daiki Ueno

;; Author: Daiki Ueno <ueno@unixuser.org>
;; Created: 1998-09-28
;; Keywords: IRC, riece, APEL

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

;;; Code:

(if (featurep 'xemacs)
    (require 'riece-xemacs)
  (require 'riece-emacs))

(defalias 'riece-mode-line-buffer-identification
  'identity)

(defun riece-simplify-mode-line-format ()
  "Remove unnecessary information from `mode-line-format'."
  (let ((value (rassq 'mode-line-modified mode-line-format)))
    (if value
	(setq mode-line-format (delq value (copy-sequence mode-line-format)))
      mode-line-format)))

(defun riece-line-beginning-position ()
  (save-excursion
    (beginning-of-line)
    (point)))

(defun riece-line-end-position ()
  (save-excursion
    (end-of-line)
    (point)))

(autoload 'read-passwd "passwd")
(defvar riece-read-passwd #'read-passwd)
(defun riece-read-passwd (prompt)
  (funcall riece-read-passwd prompt))

(if (string-match "0\\{0\\}" "")
    (defun riece-make-interval-regexp (regexp min &optional max)
      (if max
	  (format "%s\\{%d,%d\\}" regexp min max)
	(format "%s\\{%d\\}" regexp min)))
  ;; Emacs 20.7 doesn't support \{...\} in regexps.
  (defun riece-make-interval-regexp (regexp min &optional max)
    (mapconcat #'identity
	       (nconc (make-list min regexp)
		      (if max
			  (make-list (- max min) (concat regexp "?"))))
	       "")))

(if (or (not (fboundp 'make-local-hook))
	(get 'make-local-hook 'byte-obsolete-info))
    (defalias 'riece-make-local-hook 'ignore)
  (defalias 'riece-make-local-hook 'make-local-hook))

(autoload 'derived-mode-class "derived")
(if (fboundp 'derived-mode-p)
    (defalias 'riece-derived-mode-p 'derived-mode-p)
  (defun riece-derived-mode-p (&rest modes)
    (memq (derived-mode-class major-mode) modes)))

(if (fboundp 'set-process-query-on-exit-flag)
    (defalias 'riece-set-process-query-on-exit-flag
      'set-process-query-on-exit-flag)
  (defalias 'riece-set-process-query-on-exit-flag
    'process-kill-without-query))

(if (fboundp 'set-face-underline)
    (defalias 'riece-set-face-underline 'set-face-underline)
  (defalias 'riece-set-face-underline 'set-face-underline-p))

(if (fboundp 'time-less-p)
    (defalias 'riece-time-less-p 'time-less-p)
  ;; stolen (and renamed) from time-date.el.
  (defun riece-time-less-p (t1 t2)
    "Say whether time value T1 is less than time value T2."
    (or (< (car t1) (car t2))
	(and (= (car t1) (car t2))
	     (< (nth 1 t1) (nth 1 t2))))))

(if (fboundp 'time-since)
    (defalias 'riece-time-since 'time-since)
  ;; stolen (and renamed) from time-date.el.
  (defun riece-time-since (time)
    "Return the time elapsed since TIME."
    (let* ((current (current-time))
	   (rest (when (< (nth 1 current) (nth 1 time))
		   (expt 2 16))))
      (list (- (+ (car current) (if rest -1 0)) (car time))
	    (- (+ (or rest 0) (nth 1 current)) (nth 1 time))))))

(if (fboundp 'seconds-to-time)
    (defalias 'riece-seconds-to-time 'seconds-to-time)
  ;; stolen (and renamed) from time-date.el.
  (defun riece-seconds-to-time (seconds)
    "Convert SECONDS (a floating point number) to a time value."
    (list (floor seconds 65536)
	  (floor (mod seconds 65536))
	  (floor (* (- seconds (ffloor seconds)) 1000000)))))

(provide 'riece-compat)

;;; riece-compat.el ends here
