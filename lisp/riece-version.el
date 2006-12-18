;;; riece-version.el --- version information handling
;; Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003
;;        Free Software Foundation, Inc.
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

;;; Code:

(require 'riece-package-info)

;; NOTE: Most part of this file is copied from Gnus.

(defcustom riece-user-agent 'emacs-riece-type
  "Which information should be exposed in the User-Agent header.

It can be one of the symbols `riece' \(show only Riece version\), `emacs-riece'
\(show only Emacs and Riece versions\), `emacs-riece-config' \(same as
`emacs-riece' plus system configuration\), `emacs-riece-type' \(same as
`emacs-riece' plus system type\) or a custom string.  If you set it to a
string, be sure to use a valid format, see RFC 2616."
  :group 'riece-options
  :type '(choice
	  (item :tag "Show Riece and Emacs versions and system type"
		emacs-riece-type)
	  (item :tag "Show Riece and Emacs versions and system configuration"
		emacs-riece-config)
	  (item :tag "Show Riece and Emacs versions" emacs-riece)
	  (item :tag "Show only Riece version" riece)
	  (string :tag "Other")))

(defconst riece-version (format "%s v%s" riece-package-name
				riece-version-number)
  "Version string for this version of Riece.")

(eval-when-compile
  (defvar emacs-program-version)
  (defvar xemacs-codename)
  (defvar sxemacs-codename))

(defun riece-extended-version ()
  "Stringified Riece version and Emacs version.
See the variable `riece-user-agent'."
  (let* ((riece-v
	  (concat riece-package-name "/"
		  (prin1-to-string riece-version-number t)))
	 (system-v
	  (cond
	   ((eq riece-user-agent 'emacs-riece-config)
	    system-configuration)
	   ((eq riece-user-agent 'emacs-riece-type)
	    (symbol-name system-type))
	   (t nil)))
	 (emacs-v
	  (cond
	   ((eq riece-user-agent 'riece)
	    nil)
	   ((string-match "^\\(\\([.0-9]+\\)*\\)\\.[0-9]+$" emacs-version)
	    (concat "Emacs/" (match-string 1 emacs-version)
		    (if system-v
			(concat " (" system-v ")")
		      "")))
	   ((featurep 'sxemacs)
	    (concat "SXEmacs/" emacs-program-version
		    (when system-v
		      (concat " ("
			      (when sxemacs-codename
				(concat sxemacs-codename ", "))
			      system-v ")"))))
	   ((featurep 'xemacs)
	    (concat "XEmacs/" emacs-program-version
		    (when system-v
		      (concat " ("
			      (when xemacs-codename
				(concat xemacs-codename ", "))
			      system-v ")"))))
	   (t emacs-version))))
    (if (stringp riece-user-agent)
	riece-user-agent
      (concat riece-v
	      (when emacs-v
		(concat " " emacs-v))))))

(defun riece-version (&optional arg)
  "Version number of this version of Riece.
If ARG, use user-agent format."
  (interactive "P")
  (if arg
      (message "%s" (riece-extended-version))
    (message "%s" riece-version)))

(provide 'riece-version)

;;; riece-version.el ends here
