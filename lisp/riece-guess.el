;;; riece-guess.el --- guess the next channel, using multiple methods
;; Copyright (C) 1998-2003 Daiki Ueno

;; Author: Daiki Ueno <ueno@unixuser.org>
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

(require 'riece-identity)
(require 'riece-commands)

(defgroup riece-guess nil
  "Guess the next channel"
  :tag "Guess"
  :prefix "riece-"
  :group 'riece)

(defcustom riece-guess-channel-try-functions nil
  "Functions which returns a list of channels the user wants to switch."
  :type '(repeat function)
  :group 'riece-guess)

(defvar riece-current-channels)

(defun riece-guess-candidates ()
  "Build candidate list.
This function calls \\[riece-guess-channel-try-functions] in turn and
merge the results."
  (let ((functions riece-guess-channel-try-functions)
	candidates)
    (while functions
      (setq candidates
	    (nconc candidates
		   (delq nil (mapcar
			      (lambda (channel)
				(unless (riece-identity-member
					 channel candidates)
				  channel))
			      (funcall (car functions)))))
	    functions (cdr functions)))
    ;; Merge the default.
    (setq candidates
	  (nconc candidates
		 (delq nil (mapcar
			    (lambda (channel)
			      (if (and channel
				       (not (riece-identity-member
					     channel candidates)))
				  channel))
			    riece-current-channels))))
    candidates))

(defvar riece-guess-candidates nil)

(defun riece-command-guess-switch-to-channel ()
  "Try to switch to the channel where the user is interested in."
  (interactive)
  (unless (and (eq last-command this-command)
	       riece-guess-candidates)
    (setq riece-guess-candidates (riece-guess-candidates)))
  (unless riece-guess-candidates
    (error "No channel"))
  (riece-command-switch-to-channel
   (prog1 (car riece-guess-candidates)
     (setq riece-guess-candidates (cdr riece-guess-candidates)))))

(defvar riece-command-mode-map)
(defvar riece-dialogue-mode-map)
(defvar riece-channel-list-mode-map)

(defun riece-guess-insinuate ()
  (define-key riece-command-mode-map
    "\C-cg" 'riece-command-guess-switch-to-channel)
  (define-key riece-dialogue-mode-map
    "g" 'riece-command-guess-switch-to-channel)
  (define-key riece-channel-list-mode-map
    "g" 'riece-command-guess-switch-to-channel))
	
(provide 'riece-guess)

;;; riece-guess.el ends here
