;;; riece-mode.el --- functions for manipulating channel/user modes
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

(defun riece-parse-modes (string)
  (let ((start 0)
	result)
    (while (and (string-match "[-+]\\([^-+ ]*\\) *" string start)
		(= (match-beginning 0) start))
      (let ((toggle (eq (aref string start) ?+))
	    (modes (string-to-list (match-string 1 string))))
	(setq start (match-end 0))
	(while modes
	  (if (and (string-match "\\([^-+][^ ]*\\) *" string start)
		   (= (match-beginning 0) start))
	      (setq start (match-end 0)
		    result (cons (list (riece-make-mode
					(car modes) (match-string 1 string))
				       toggle)
				 result))
	    (setq result (cons (list (riece-make-mode (car modes))
				     toggle)
			       result)))
	  (setq modes (cdr modes)))))
    (nreverse result)))

(defun riece-mode-assoc (flag modes)
  "Return a mode object matched with FLAG in MODES."
  (catch 'found
    (while modes
      (if (eq flag (riece-mode-flag (car modes)))
	  (throw 'found (car modes)))
      (setq modes (cdr modes)))))

(defun riece-make-mode (flag &optional parameter)
  "Make an instance of mode object.
Arguments are appropriate to the flag and the parameter."
  (vector flag parameter))

(defun riece-mode-flag (mode)
  "Return the flag of MODE."
  (aref mode 0))

(defun riece-mode-parameter (mode)
  "Return the parameter of MODE."
  (aref mode 1))

(provide 'riece-mode)

;;; riece-mode.el ends here