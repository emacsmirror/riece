;;; riece.el --- IRC client for Emacsen
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

(if (featurep 'xemacs)
    (require 'riece-xemacs)
  (require 'riece-emacs))

(require 'riece-filter)
(require 'riece-display)
(require 'riece-server)
(require 'riece-compat)
(require 'riece-commands)

(defvar riece-channel-list-mode-map (make-sparse-keymap))
(defvar riece-user-list-mode-map (make-sparse-keymap))

(defvar riece-dialogue-mode-map
  (let ((keymap (make-keymap)))
    (suppress-keymap keymap 'nodigit)
    keymap))

(defvar riece-command-mode-map (make-keymap))
(defvar riece-command-map (make-sparse-keymap))

(defvar riece-command-mode-syntax-table nil)

(put 'riece-command-mode 'mode-class 'special)
(put 'riece-dialogue-mode 'mode-class 'special)
(put 'riece-channel-list-mode 'mode-class 'special)
(put 'riece-user-list-mode 'mode-class 'special)
(put 'riece-channel-mode 'derived-mode-parent 'riece-dialogue-mode)
(put 'riece-others-mode 'derived-mode-parent 'riece-dialogue-mode)

(defvar riece-buffer-mode-alist
  '((riece-dialogue-buffer . riece-dialogue-mode)
    (riece-others-buffer . riece-others-mode)
    (riece-user-list-buffer . riece-user-list-mode)
    (riece-channel-list-buffer . riece-channel-list-mode)
    (riece-private-buffer . riece-dialogue-mode)
    (riece-wallops-buffer)))
    
(defvar riece-select-keys
  '("1" riece-command-switch-to-channel-by-number-1
    "2" riece-command-switch-to-channel-by-number-2
    "3" riece-command-switch-to-channel-by-number-3
    "4" riece-command-switch-to-channel-by-number-4
    "5" riece-command-switch-to-channel-by-number-5
    "6" riece-command-switch-to-channel-by-number-6
    "7" riece-command-switch-to-channel-by-number-7
    "8" riece-command-switch-to-channel-by-number-8
    "9" riece-command-switch-to-channel-by-number-9
    "0" riece-command-switch-to-channel-by-number-10
    "\C-c1" riece-command-switch-to-channel-by-number-11
    "\C-c2" riece-command-switch-to-channel-by-number-12
    "\C-c3" riece-command-switch-to-channel-by-number-13
    "\C-c4" riece-command-switch-to-channel-by-number-14
    "\C-c5" riece-command-switch-to-channel-by-number-15
    "\C-c6" riece-command-switch-to-channel-by-number-16
    "\C-c7" riece-command-switch-to-channel-by-number-17
    "\C-c8" riece-command-switch-to-channel-by-number-18
    "\C-c9" riece-command-switch-to-channel-by-number-19
    "\C-c0" riece-command-switch-to-channel-by-number-20))

;;; Keymap macros. -- borrowed from `gnus-util.el'.
(defmacro riece-local-set-keys (&rest plist)
  "Set the keys in PLIST in the current keymap."
  `(riece-define-keys-1 (current-local-map) ',plist))

(defmacro riece-define-keys (keymap &rest plist)
  "Assign KEYMAP keys from PLIST."
  `(riece-define-keys-1 ',keymap ',plist))

(defmacro riece-define-keys-safe (keymap &rest plist)
  "Assign KEYMAP keys from PLIST without overwriting previous definitions."
  `(riece-define-keys-1 ',keymap ',plist t))

(put 'riece-define-keys 'lisp-indent-function 1)
(put 'riece-define-keys-safe 'lisp-indent-function 1)
(put 'riece-local-set-keys 'lisp-indent-function 1)

(defun riece-define-keys-1 (keymap plist &optional safe)
  "Assign KEYMAP keys from PLIST.
If optional argument SAFE is nil, overwrite previous definitions."
  (unless keymap
    (error "Can't set keys in a null keymap"))
  (cond
   ((symbolp keymap)
    (setq keymap (symbol-value keymap)))
   ((keymapp keymap))
   ((listp keymap)
    (set (car keymap) nil)
    (define-prefix-command (car keymap))
    (define-key (symbol-value (nth 2 keymap)) (nth 1 keymap) (car keymap))
    (setq keymap (symbol-value (car keymap)))))
  (let (key)
    (while plist
      (if (symbolp (setq key (car plist)))
	  (setq key (symbol-value key)))
      (setq plist (cdr plist))
      (if (or (not safe)
	      (eq (lookup-key keymap key) 'undefined))
	  (define-key keymap key (car plist))
	(car plist))
      (setq plist (cdr plist)))))

(when t
  (riece-define-keys riece-dialogue-mode-map
    "\177" scroll-down
    [delete] scroll-down
    [backspace] scroll-down
    [return] scroll-up
    " " scroll-up
    "$" end-of-buffer
    "/" riece-command-raw
    ">" end-of-buffer
    "<" beginning-of-buffer
    "\C-ta" riece-command-toggle-away
    "c" riece-command-select-command-buffer
    "f" riece-command-finger
    "\C-tf" riece-command-toggle-freeze
    "\C-to" riece-command-toggle-own-freeze
    "\C-tu" riece-command-toggle-user-list-buffer-mode
    "\C-tc" riece-command-toggle-channel-buffer-mode
    "\C-tC" riece-command-toggle-channel-list-buffer-mode
    "i" riece-command-invite
    "j" riece-command-join
    "\C-k" riece-command-kick
    "l" riece-command-list
    "m" riece-dialogue-enter-message
    "M" riece-command-change-mode
    "n" riece-command-change-nickname
    "\C-n" riece-command-names
    "o" other-window
    "O" riece-command-open-server
    "C" riece-command-close-server
    "M" riece-command-universal-server-name-argument
    "q" riece-command-quit
    "r" riece-command-configure-windows
    "x" riece-command-copy-region
    "t" riece-command-topic
    "w" riece-command-who)

  (riece-define-keys riece-command-mode-map
    "\r" riece-command-enter-message
    [(control return)] riece-command-enter-message-as-notice)

  (riece-define-keys (riece-command-map "\C-c" riece-command-mode-map)
    "\177" riece-command-scroll-down
    [delete] riece-command-scroll-down
    [backspace] riece-command-scroll-down
    " " riece-command-scroll-up
    "$" riece-command-end-of-buffer
    ">" riece-command-next-channel
    "<" riece-command-previous-channel
    "\C-j" riece-command-next-channel
    "\C-n" riece-command-names
    "l" riece-command-list
    "\C-m" riece-command-change-mode
    "o" riece-command-set-operators
    "\C-p" riece-command-part
    "r" riece-command-configure-windows
    "v" riece-command-set-speakers)
  (set-keymap-parent riece-command-map riece-dialogue-mode-map)

  (riece-define-keys riece-user-list-mode-map
    "o" riece-command-set-operators
    "v" riece-command-set-voices
    "f" riece-command-finger
    " " riece-command-nick-scroll-up
    "\177" riece-command-nick-scroll-down
    [delete] riece-command-nick-scroll-down
    [backspace] riece-command-nick-scroll-down
    "c" riece-command-select-command-buffer)

  (riece-define-keys riece-channel-list-mode-map
    ">" riece-command-next-channel
    "<" riece-command-previous-channel
    "o" other-window
    "c" riece-command-select-command-buffer)

  (riece-define-keys-1 riece-dialogue-mode-map riece-select-keys)
  (riece-define-keys-1 riece-channel-list-mode-map riece-select-keys))

(defun riece-read-variables-files (&optional file)
  "Read variables FILEs."
  (or (file-directory-p riece-directory)
      (make-directory riece-directory))
  (let ((files (if file
		   (setq riece-variables-file file
			 riece-variables-files (list file))
		 riece-variables-files)))
    (while files
      (condition-case nil
	  (load (expand-file-name (car files)))
	(file-error nil))
      (setq files (cdr files)))))

(defvar print-quoted)
(defvar print-escape-multibyte)
(defun riece-save-variables-files ()
  "Save current settings to `riece-variables-file'."
  (with-temp-file riece-saved-variables-file
    (let ((print-quoted t)
	  (print-readably t)
	  print-escape-multibyte
	  print-level
	  print-length
	  (variables riece-saved-forms))
      (while variables
	(prin1 `(setq ,(car variables)
		      ',(symbol-value (car variables)))
	       (current-buffer))
	(insert "\n")
	(setq variables (cdr variables)))))
  (setq riece-save-variables-are-dirty nil))

;;;###autoload
(defun riece (&optional confirm)
  "Connect to the IRC server and start chatting.
If optional argument CONFIRM is non-nil, ask which IRC server to connect.
If already connected, just pop up the windows."
  (interactive "P")
  (riece-read-variables-files (if noninteractive
				  (car command-line-args-left)))
  (riece-insinuate-addons riece-addons)
  (run-hooks 'riece-after-load-startup-hook)
  (if (riece-server-opened)
      (riece-configure-windows)
    (switch-to-buffer (riece-get-buffer-create riece-command-buffer))
    (unless (eq major-mode 'riece-command-mode)
      (riece-command-mode))
    (if (or confirm (null riece-server))
	(setq riece-server (completing-read "Server: " riece-server-alist)))
    (if (stringp riece-server)
	(setq riece-server (riece-server-name-to-server riece-server)))
    (riece-create-buffers)
    (riece-configure-windows)
    (riece-open-server riece-server "")
    (run-hooks 'riece-startup-hook)
    (message "%s" (substitute-command-keys
		   "Type \\[describe-mode] for help"))))

(defun riece-exit ()
  (setq riece-server nil)
  (if riece-save-variables-are-dirty
      (riece-save-variables-files))
  (riece-clear-system)
  (run-hooks 'riece-exit-hook))

(defun riece-command-mode ()
  "Major mode for Riece.  Normal edit function are available.
Typing Return or Linefeed enters the current line in the dialogue.
The following special commands are available:
For a list of the generic commands type \\[riece-command-generic] ? RET.
\\{riece-command-mode-map}"
  (interactive)
  (kill-all-local-variables)

  (setq riece-away-indicator "-"
	riece-operator-indicator "-"
	major-mode 'riece-command-mode
	mode-name "Commands"
	mode-line-buffer-identification
	(riece-mode-line-buffer-identification
	 '("Riece: "
	   riece-away-indicator
	   riece-operator-indicator
	   riece-freeze-indicator
	   " "
	   riece-user-indicator
	   " "
	   riece-short-channel-indicator)))
  (riece-simplify-mode-line-format)
  (use-local-map riece-command-mode-map)

  (unless riece-command-mode-syntax-table
    (setq riece-command-mode-syntax-table
	  (copy-syntax-table (syntax-table)))
    (set-syntax-table riece-command-mode-syntax-table)
    (mapcar
     (lambda (c) (modify-syntax-entry c "w"))
     "^[]{}'`"))

  (run-hooks 'riece-command-mode-hook))
  
(defun riece-dialogue-mode ()
  "Major mode for displaying the IRC dialogue.
All normal editing commands are turned off.
Instead, these commands are available:
\\{riece-dialogue-mode-map}"
  (kill-all-local-variables)
  (make-local-variable 'riece-freeze)
  (make-local-variable 'tab-stop-list)
  (setq riece-freeze riece-default-freeze
	riece-away-indicator "-"
	riece-operator-indicator "-"
	major-mode 'riece-dialogue-mode
	mode-name "Dialogue"
	mode-line-buffer-identification
	(riece-mode-line-buffer-identification
	 '("Riece: "
	   riece-away-indicator
	   riece-operator-indicator
	   riece-freeze-indicator
	   " "
	   riece-channel-list-indicator " "))
	buffer-read-only t
	tab-stop-list riece-tab-stop-list)
  (riece-simplify-mode-line-format)
  (use-local-map riece-dialogue-mode-map)
  (buffer-disable-undo)
  (run-hooks 'riece-dialogue-mode-hook))

(define-derived-mode riece-others-mode riece-dialogue-mode
  "Others"
  "Major mode for displaying the IRC others message except current channel.
All normal editing commands are turned off.
Instead, these commands are available:
\\{riece-others-mode-map}")

(define-derived-mode riece-channel-mode riece-dialogue-mode
  "Channel"
  "Major mode for displaying the IRC current channel buffer.
All normal editing commands are turned off.
Instead, these commands are available:
\\{riece-channel-mode-map}"
  (setq mode-line-buffer-identification
	(riece-mode-line-buffer-identification
	 '("Riece: "
	   riece-away-indicator
	   riece-operator-indicator
	   riece-freeze-indicator
	   " "
	   riece-channel-indicator))))

(defun riece-channel-list-mode ()
  "Major mode for displaying channel list.
All normal editing commands are turned off."
  (kill-all-local-variables)
  (buffer-disable-undo)
  (make-local-variable 'riece-redisplay-buffer)
  (setq major-mode 'riece-channel-list-mode
        mode-name "Channels"
	mode-line-buffer-identification
	(riece-mode-line-buffer-identification '("Riece: "))
	truncate-lines t
	buffer-read-only t)
  (use-local-map riece-channel-list-mode-map)
  (run-hooks 'riece-channel-list-mode-hook))

(defun riece-user-list-mode ()
  "Major mode for displaying members in the IRC current channel buffer.
All normal editing commands are turned off.
Instead, these commands are available:
\\{riece-user-list-mode-map}"
  (kill-all-local-variables)
  (buffer-disable-undo)
  (make-local-variable 'riece-redisplay-buffer)
  (setq major-mode 'riece-user-list-mode
        mode-name "User list"
	mode-line-buffer-identification
	(riece-mode-line-buffer-identification
	 '("Riece: " riece-channel-indicator " "))
	truncate-lines t
	buffer-read-only t)
  (if (boundp 'transient-mark-mode)
      (set (make-local-variable 'transient-mark-mode) t))
  (use-local-map riece-user-list-mode-map)
  (run-hooks 'riece-user-list-mode-hook))

(defun riece-create-buffers ()
  (let ((alist riece-buffer-mode-alist))
    (while alist
      (save-excursion
	(set-buffer (riece-get-buffer-create
		     (symbol-value (car (car alist)))))
	(unless (or (null (cdr (car alist)))
		    (eq major-mode (cdr (car alist))))
	  (funcall (cdr (car alist))))
	(setq alist (cdr alist))))))

(defun riece-load-and-build-addon-dependencies (addons)
  (let ((load-path (cons riece-addon-directory load-path))
	dependencies)
    (while addons
      (require (car addons))		;error will be reported here
      (let* ((requires
	      (funcall (or (intern-soft
			    (concat (symbol-name (car addons)) "-requires"))
			   #'ignore)))
	     (pointer requires)
	     entry)
	;; Increment succs' pred count.
	(if (setq entry (assq (car addons) dependencies))
	    (setcar (cdr entry) (+ (length requires) (nth 1 entry)))
	  (setq dependencies (cons (list (car addons) (length requires))
				   dependencies)))
	;; Merge pred's succs.
	(while pointer
	  (if (setq entry (assq (car pointer) dependencies))
	      (setcdr (cdr entry)
		      (cons (car addons) (nthcdr 2 entry)))
	    (setq dependencies (cons (list (car pointer) 0 (car addons))
				     dependencies)))
	  (setq pointer (cdr pointer))))
      (setq addons (cdr addons)))
    dependencies))

(defun riece-insinuate-addons (addons)
  (let ((pointer addons)
	dependencies queue)
    ;; Uniquify, first.
    (while pointer
      (if (memq (car pointer) (cdr pointer))
	  (setcar pointer nil))
      (setq pointer (cdr pointer)))
    (setq dependencies (riece-load-and-build-addon-dependencies
			(delq nil addons))
	  pointer dependencies)
    ;; Sort them.
    (while pointer
      (if (zerop (nth 1 (car pointer)))
	  (setq dependencies (delq (car pointer) dependencies)
		queue (cons (car pointer) queue)))
      (setq pointer (cdr pointer)))
    (setq addons nil)
    (while queue
      (setq addons (cons (car (car queue)) addons)
	    pointer (nthcdr 2 (car queue)))
      (while pointer
	(let* ((entry (assq (car pointer) dependencies))
	       (count (1- (nth 1 entry))))
	  (if (zerop count)
	      (progn
		(setq dependencies (delq entry dependencies)
		      queue (nconc queue (list entry))))
	    (setcar (cdr entry) count)))
	(setq pointer (cdr pointer)))
      (setq queue (cdr queue)))
    (if dependencies
	(error "Circular add-on dependency found"))
    (while addons
      (require (car addons))		;implicit dependency
      (funcall (intern (concat (symbol-name (car addons)) "-insinuate")))
      (setq addons (cdr addons)))))

(provide 'riece)

;;; riece.el ends here
