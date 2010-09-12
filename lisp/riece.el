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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'riece-display)
(require 'riece-server)
(require 'riece-compat)
(require 'riece-commands)
(require 'riece-addon)
(require 'riece-signal)

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
(put 'riece-others-mode 'derived-mode-parent 'riece-dialogue-mode)
(put 'riece-channel-list-mode 'mode-class 'special)
(put 'riece-user-list-mode 'mode-class 'special)
(put 'riece-channel-mode 'derived-mode-parent 'riece-dialogue-mode)

(defvar riece-buffer-alist
  '((riece-command-buffer "*Command*" riece-command-mode)
    (riece-dialogue-buffer "*Dialogue*" riece-dialogue-mode)
    (riece-others-buffer "*Others*" riece-others-mode)
    (riece-channel-list-buffer "*Channels*" riece-channel-list-mode)
    (riece-user-list-buffer " *Users*" riece-user-list-mode)
    (riece-temp-buffer " *Temp*")
    (riece-debug-buffer "*Debug*")))

(defvar riece-select-keys
  `("#" riece-command-switch-to-channel-by-number
    "1" riece-command-switch-to-channel-by-number-1
    "2" riece-command-switch-to-channel-by-number-2
    "3" riece-command-switch-to-channel-by-number-3
    "4" riece-command-switch-to-channel-by-number-4
    "5" riece-command-switch-to-channel-by-number-5
    "6" riece-command-switch-to-channel-by-number-6
    "7" riece-command-switch-to-channel-by-number-7
    "8" riece-command-switch-to-channel-by-number-8
    "9" riece-command-switch-to-channel-by-number-9
    "0" riece-command-switch-to-channel-by-number-10
    ,(concat riece-command-prefix "1")
    riece-command-switch-to-channel-by-number-11
    ,(concat riece-command-prefix "2")
    riece-command-switch-to-channel-by-number-12
    ,(concat riece-command-prefix "3")
    riece-command-switch-to-channel-by-number-13
    ,(concat riece-command-prefix "4")
    riece-command-switch-to-channel-by-number-14
    ,(concat riece-command-prefix "5")
    riece-command-switch-to-channel-by-number-15
    ,(concat riece-command-prefix "6")
    riece-command-switch-to-channel-by-number-16
    ,(concat riece-command-prefix "7")
    riece-command-switch-to-channel-by-number-17
    ,(concat riece-command-prefix "8")
    riece-command-switch-to-channel-by-number-18
    ,(concat riece-command-prefix "9")
    riece-command-switch-to-channel-by-number-19
    ,(concat riece-command-prefix "0")
    riece-command-switch-to-channel-by-number-20))

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
    (define-key (symbol-value (nth 2 keymap))
      (if (symbolp (nth 1 keymap))
	  (symbol-value (nth 1 keymap))
	(nth 1 keymap))
      (car keymap))
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
    [home] beginning-of-buffer
    "$" end-of-buffer
    [end] end-of-buffer
    "/" riece-command-raw
    ">" end-of-buffer
    "<" beginning-of-buffer
    "^" riece-command-list-addons
    "\C-ta" riece-command-toggle-away
    "c" riece-command-select-command-buffer
    "f" riece-command-finger
    "\C-tf" riece-command-toggle-freeze
    "\C-to" riece-command-toggle-own-freeze
    "\C-tO" riece-command-toggle-others-buffer-mode
    "\C-tu" riece-command-toggle-user-list-buffer-mode
    "\C-tc" riece-command-toggle-channel-buffer-mode
    "\C-tC" riece-command-toggle-channel-list-buffer-mode
    "\C-tl" riece-command-change-layout
    "i" riece-command-invite
    "j" riece-command-join
    "\C-k" riece-command-kick
    "l" riece-command-list
    "M" riece-command-change-mode
    "n" riece-command-change-nickname
    "N" riece-command-names
    "o" other-window
    "O" riece-command-open-server
    "C" riece-command-close-server
    "M" riece-command-universal-server-name-argument
    "p" riece-command-enter-message-to-user
    "q" riece-command-quit
    "r" riece-command-configure-windows
    "s" riece-command-save-variables
    "x" riece-command-copy-region
    "t" riece-command-topic
    "w" riece-command-who
    "z" riece-command-suspend-resume)

  (riece-define-keys riece-command-mode-map
    "\r" riece-command-enter-message
    [(control return)] riece-command-enter-message-as-notice
    [tab] riece-command-complete-user)

  (riece-define-keys (riece-command-map riece-command-prefix
					riece-command-mode-map)
    "\177" riece-command-scroll-down
    [delete] riece-command-scroll-down
    [backspace] riece-command-scroll-down
    " " riece-command-scroll-up
    [home] riece-command-beginning-of-buffer
    "$" riece-command-end-of-buffer
    [end] riece-command-end-of-buffer
    ">" riece-command-next-channel
    "<" riece-command-previous-channel
    "\C-j" riece-command-next-channel
    "\C-n" riece-command-names
    "l" riece-command-list
    "\C-m" riece-command-change-mode
    "o" riece-command-set-operators
    "\C-p" riece-command-part
    "r" riece-command-configure-windows
    "v" riece-command-set-speakers
    "V" riece-version)
  (set-keymap-parent riece-command-map riece-dialogue-mode-map)

  (riece-define-keys riece-user-list-mode-map
    "o" riece-command-set-operators
    "v" riece-command-set-voices
    "f" riece-command-finger
    " " riece-command-user-list-scroll-up
    "\177" riece-command-user-list-scroll-down
    [delete] riece-command-user-list-scroll-down
    [backspace] riece-command-user-list-scroll-down
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
  "Save current settings to `riece-saved-variables-file'."
  (message (riece-mcat "Saving %s...") riece-saved-variables-file)
  (with-temp-file riece-saved-variables-file
    (insert ";;; This file is generated automatically by " riece-version ".\n"
	    ";;; Do not edit this file!\n\n")
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
  (message (riece-mcat "Saving %s...done") riece-saved-variables-file)
  (setq riece-save-variables-are-dirty nil))

;;;###autoload
(defun riece (&optional confirm)
  "Connect to the IRC server and start chatting.
If optional argument CONFIRM is non-nil, ask which IRC server to connect."
  (interactive "P")
  (riece-read-variables-files (if noninteractive
				  (car command-line-args-left)))
  (run-hooks 'riece-after-load-startup-hook)
  (if (riece-server-opened)
      (riece-command-configure-windows)
    (modify-frame-parameters (selected-frame)
			     (list (cons 'riece-window-configuration
					 (current-window-configuration))))
    (setq riece-addon-dependencies (riece-resolve-addons
				    (copy-sequence riece-addons)))
    (let ((pointer riece-addon-dependencies))
      (while pointer
	(riece-insinuate-addon (car (car pointer)) riece-debug)
	(setq pointer (cdr pointer))))
    (if (or confirm (null riece-server))
	(setq riece-server (completing-read (riece-mcat "Server: ")
					    riece-server-alist)))
    (if (stringp riece-server)
	(setq riece-server (riece-server-name-to-server riece-server)))
    (riece-create-buffers)
    (switch-to-buffer riece-command-buffer)
    (riece-display-connect-signals)
    (riece-redisplay-buffers)
    (riece-open-server riece-server "")
    ;; If no server process is available, exit.
    (if (null riece-server-process-alist)
	(riece-exit)
      (let ((server-list riece-startup-server-list))
	(while server-list
	  (riece-command-open-server (car server-list))
	  (setq server-list (cdr server-list))))
      (let ((channel-list riece-startup-channel-list)
	    server)
	(while channel-list
	  (setq server (riece-identity-server
			(riece-parse-identity (car channel-list))))
	  (unless (riece-server-opened server)
	    (riece-command-open-server server))
	  (setq channel-list (cdr channel-list))))
      (let ((pointer riece-addon-dependencies))
	(while pointer
	  (unless (get (car (car pointer)) 'riece-addon-default-disabled)
	    (riece-enable-addon (car (car pointer)) riece-debug))
	  (setq pointer (cdr pointer))))
      (run-hooks 'riece-startup-hook)
      (message "%s" (substitute-command-keys
		     (riece-mcat "Type \\[describe-mode] for help"))))))

(defun riece-exit ()
  (if riece-save-variables-are-dirty
      (riece-save-variables-files))
  (while riece-buffer-list
    (if (and (get-buffer (car riece-buffer-list))
	     (buffer-live-p (car riece-buffer-list)))
	(funcall riece-buffer-dispose-function (car riece-buffer-list)))
    (setq riece-buffer-list (cdr riece-buffer-list)))
  (riece-clear-signal-slots)
  (setq riece-server nil
	riece-current-channels nil
	riece-current-channel nil
	riece-channel-buffer nil
	riece-channel-buffer-alist nil
	riece-user-indicator nil
	riece-long-channel-indicator (riece-mcat "None")
	riece-channel-list-indicator (riece-mcat "No channel")
	riece-away-indicator "-"
	riece-operator-indicator "-"
	riece-channel-status-indicator "-"
	riece-freeze-indicator "-")
  (modify-frame-parameters (selected-frame)
			   (list (list 'riece-window-configuration)))
  (delete-other-windows)
  (run-hooks 'riece-exit-hook))

(defun riece-command-mode ()
  "Major mode for Riece.  Normal edit function are available.
Typing Return or Linefeed enters the current line in the dialogue.
The following special commands are available:
For a list of the generic commands type \\[riece-command-generic] ? RET.
\\{riece-command-mode-map}"
  (interactive)
  (kill-all-local-variables)

  ;; Make `truncate-partial-width-windows' buffer local and set it to
  ;; nil.  This causes `truncate-lines' to directly control line
  ;; truncation.
  (make-local-variable 'truncate-partial-width-windows)
  (setq truncate-partial-width-windows nil)

  (make-local-variable 'riece-mode-line-buffer-identification)
  (setq riece-away-indicator "-"
	riece-operator-indicator "-"
	riece-channel-status-indicator "-"
	major-mode 'riece-command-mode
	mode-name "Command"
	riece-mode-line-buffer-identification
	'("Riece: "
	  riece-away-indicator
	  riece-operator-indicator
	  riece-channel-status-indicator
	  " "
	  riece-user-indicator
	  " "
	  riece-channel-indicator)
	mode-line-buffer-identification
	(riece-mode-line-buffer-identification
	 riece-mode-line-buffer-identification)
	truncate-lines nil)
  (riece-simplify-mode-line-format)
  (use-local-map riece-command-mode-map)

  (unless riece-command-mode-syntax-table
    (setq riece-command-mode-syntax-table
	  (copy-syntax-table (syntax-table)))
    (set-syntax-table riece-command-mode-syntax-table)
    (let* ((chars "^[]{}'`")
	   (length (length chars))
	   (index 0))
      (while (< index length)
	(modify-syntax-entry (aref chars index) "w")
	(setq index (1+ index)))))

  (run-hooks 'riece-command-mode-hook))

(defun riece-dialogue-mode ()
  "Major mode for displaying the IRC dialogue.
All normal editing commands are turned off.
Instead, these commands are available:
\\{riece-dialogue-mode-map}"
  (kill-all-local-variables)
  (make-local-variable 'riece-freeze)
  (make-local-variable 'riece-freeze-indicator)

  ;; Make `truncate-partial-width-windows' buffer local and set it to
  ;; nil.  This causes `truncate-lines' to directly control line truncation.
  (make-local-variable 'truncate-partial-width-windows)
  (setq truncate-partial-width-windows nil)

  (make-local-variable 'riece-mode-line-buffer-identification)
  (setq riece-freeze riece-default-freeze
	riece-away-indicator "-"
	riece-operator-indicator "-"
	riece-channel-status-indicator "-"
	major-mode 'riece-dialogue-mode
	mode-name "Dialogue"
	riece-mode-line-buffer-identification
	'("Riece: "
	  riece-away-indicator
	  riece-operator-indicator
	  riece-freeze-indicator
	  riece-channel-status-indicator
	  " "
	  riece-channel-list-indicator " ")
	mode-line-buffer-identification
	(riece-mode-line-buffer-identification
	 riece-mode-line-buffer-identification)
	truncate-lines nil
	buffer-read-only t)
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
  (make-local-variable 'riece-channel-buffer-window-point)
  (make-local-variable 'riece-mode-line-buffer-identification)
  (setq riece-mode-line-buffer-identification
	'("Riece: "
	  riece-away-indicator
	  riece-operator-indicator
	  riece-freeze-indicator
	  riece-channel-status-indicator
	  " "
	  riece-long-channel-indicator)
	mode-line-buffer-identification
	(riece-mode-line-buffer-identification
	 riece-mode-line-buffer-identification)))

(defun riece-channel-list-mode ()
  "Major mode for displaying channel list.
All normal editing commands are turned off."
  (kill-all-local-variables)
  (buffer-disable-undo)

  ;; Make `truncate-partial-width-windows' buffer local and set it to
  ;; nil.  This causes `truncate-lines' to directly control line truncation.
  (make-local-variable 'truncate-partial-width-windows)
  (setq truncate-partial-width-windows nil)

  (make-local-variable 'riece-mode-line-buffer-identification)
  (setq major-mode 'riece-channel-list-mode
	mode-name "Channels"
	riece-mode-line-buffer-identification '("Riece: ")
	mode-line-buffer-identification
	(riece-mode-line-buffer-identification
	 riece-mode-line-buffer-identification)
	truncate-lines t
	buffer-read-only t)
  (riece-make-local-hook 'riece-update-buffer-functions)
  (add-hook 'riece-update-buffer-functions
	    'riece-update-channel-list-buffer nil t)
  (use-local-map riece-channel-list-mode-map)
  (run-hooks 'riece-channel-list-mode-hook))

(defun riece-user-list-mode ()
  "Major mode for displaying members in the IRC current channel buffer.
All normal editing commands are turned off.
Instead, these commands are available:
\\{riece-user-list-mode-map}"
  (kill-all-local-variables)
  (buffer-disable-undo)

  ;; Make `truncate-partial-width-windows' buffer local and set it to
  ;; nil.  This causes `truncate-lines' to directly control line truncation.
  (make-local-variable 'truncate-partial-width-windows)
  (setq truncate-partial-width-windows nil)

  (make-local-variable 'riece-mode-line-buffer-identification)
  (setq major-mode 'riece-user-list-mode
	mode-name "Users"
	riece-mode-line-buffer-identification
	'("Riece: " riece-long-channel-indicator " ")
	mode-line-buffer-identification
	(riece-mode-line-buffer-identification
	 riece-mode-line-buffer-identification)
	truncate-lines t
	buffer-read-only t)
  (if (boundp 'transient-mark-mode)
      (set (make-local-variable 'transient-mark-mode) t))
  (riece-make-local-hook 'riece-update-buffer-functions)
  (add-hook 'riece-update-buffer-functions
	    'riece-update-user-list-buffer nil t)
  (use-local-map riece-user-list-mode-map)
  (run-hooks 'riece-user-list-mode-hook))

(defun riece-create-buffers ()
  (let ((alist riece-buffer-alist))
    (while alist
      (with-current-buffer (apply #'riece-get-buffer-create
				  (cdr (car alist)))
	(set (car (car alist)) (current-buffer))
	(unless (or (null (nth 2 (car alist)))
		    (eq major-mode (nth 2 (car alist))))
	  (funcall (nth 2 (car alist))))
	(setq alist (cdr alist))))))

(defun riece-submit-bug-report ()
  "Submit via mail a bug report on Riece."
  (interactive)
  (browse-url "https://savannah.nongnu.org/bugs/?group=riece"))

(provide 'riece)

;;; riece.el ends here
