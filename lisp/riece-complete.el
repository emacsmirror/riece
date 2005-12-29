;;; riece-complete.el --- completion
;; Copyright (C) 1998-2003 Daiki Ueno

;; Author: Daiki Ueno <ueno@unixuser.org>
;; Created: 1999-02-02
;; Keywords: minibuffer, completion

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

(eval-when-compile (require 'cl))	;butlast

(require 'riece-compat)

(defvar riece-minibuffer-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map "?" 'riece-minibuffer-help)
    map))

(defvar riece-temp-minibuffer-message nil)

;; stolen (and renamed) from XEmacs's minibuf.el.
(defun riece-temp-minibuffer-message (message)
  (let ((end (point-max)))
    (save-excursion
      (goto-char (point-max))
      (message nil)
      (insert message))
    (let ((inhibit-quit t))
      (sit-for 2)
      (delete-region end (point-max)))))

(defun riece-minibuffer-help ()
  (interactive)
  (if riece-temp-minibuffer-message
      (riece-temp-minibuffer-message riece-temp-minibuffer-message)))

;;; stolen (and renamed) from crm.el.
(defvar riece-completing-read-multiple-separator ",")
(defvar riece-completing-read-multiple-table nil)

(defun riece-completing-read-multiple-1 (string predicate flag)
  "Function used by `riece-completing-read-multiple'.
The value of STRING is the string to be completed.

The value of PREDICATE is a function to filter possible matches, or
nil if none.

The value of FLAG is used to specify the type of completion operation.
A value of nil specifies `try-completion'.  A value of t specifies
`all-completions'.  A value of lambda specifes a test for an exact match.

For more information on STRING, PREDICATE, and FLAG, see the Elisp
Reference sections on 'Programmed Completion' and 'Basic Completion
Functions'."
  (let ((except (split-string string riece-completing-read-multiple-separator))
	(table (copy-sequence riece-completing-read-multiple-table))
	lead)
    ;; Remove a partially matched word construct if it exists.
    (or (string-match
	 (concat riece-completing-read-multiple-separator "$")
	 string)
	(setq except (butlast except)))
    (when (string-match
	   (concat ".*" riece-completing-read-multiple-separator)
	   string)
      (setq lead (substring string 0 (match-end 0))
	    string (substring string (match-end 0))))
    (while except
      (let ((entry (assoc (car except) table)))
	(if entry
	    (setq table (delq entry table)))
	(setq except (cdr except))))
    (if (null flag)
	(progn
	  (setq string (try-completion string table predicate))
	  (or (eq t string)
	      (concat lead string)))
      (if (eq flag 'lambda)
	  (eq t (try-completion string table predicate))
	(if flag
	    (all-completions string table predicate))))))

(defun riece-completing-read-multiple
  (prompt table &optional predicate require-match initial-input
	  history default)
  "Execute `completing-read' consequently.

See the documentation for `completing-read' for details on the arguments:
PROMPT, TABLE, PREDICATE, REQUIRE-MATCH, INITIAL-INPUT, HISTORY, DEFAULT."
  (let ((prompt
	 (format "%s (separated by \"%s\"): "
		 prompt riece-completing-read-multiple-separator))
	(riece-completing-read-multiple-table table))
    (split-string
     (completing-read
      prompt #'riece-completing-read-multiple-1
      predicate require-match initial-input history default)
     riece-completing-read-multiple-separator)))

(provide 'riece-complete)

;;; riece-complete.el ends here
