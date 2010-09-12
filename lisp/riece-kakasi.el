;;; riece-kakasi.el --- convert Japanese to roman string by KAKASI
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

(defconst riece-kakasi-description
  "Convert Japanese to roman string by KAKASI.")

(defvar riece-kakasi-process nil)

(require 'riece-message)

(defun riece-kakasi-convert-string (string)
  (process-send-string riece-kakasi-process (concat string "\n"))
  (with-current-buffer (process-buffer riece-kakasi-process)
    (while (progn
	     (goto-char (point-min))
	     (not (search-forward "\n" nil t)))
      (accept-process-output riece-kakasi-process))
    (prog1 (buffer-substring (point-min) (1- (point)))
      (delete-region (point-min) (point)))))

(defun riece-kakasi-message-filter (message)
  (if (get 'riece-kakasi 'riece-addon-enabled)
      (riece-message-set-text message
			      (riece-kakasi-convert-string
			       (riece-message-text message))))
  message)

(defun riece-kakasi-insinuate ()
  (add-hook 'riece-message-filter-functions 'riece-kakasi-message-filter))

(defun riece-kakasi-uninstall ()
  (remove-hook 'riece-message-filter-functions 'riece-kakasi-message-filter))

(defun riece-kakasi-enable ()
  (setq riece-kakasi-process
	(start-process "kakasi" (generate-new-buffer " *riece-kakasi*")
		       "kakasi" "-Ha" "-Ka" "-Ja" "-Ea" "-ka"))
  (with-current-buffer (process-buffer riece-kakasi-process)
    (buffer-disable-undo)
    (erase-buffer)))

(defun riece-kakasi-disable ()
  (kill-buffer (process-buffer riece-kakasi-process)))

(provide 'riece-kakasi)

;;; riece-kakasi.el ends here
