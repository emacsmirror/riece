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

(require 'riece-filter)
(require 'riece-display)
(require 'riece-server)
(require 'riece-compat)
(require 'riece-commands)
(require 'riece-addon)

(autoload 'derived-mode-class "derived")

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
    (riece-user-list-buffer " *Users*" riece-user-list-mode)))

(defvar riece-shrink-buffer-idle-timer nil
  "Timer object to periodically shrink channel buffers.")

(defvar riece-addons-insinuated nil
  "Non nil if add-ons are already insinuated.")

(defvar riece-select-keys
  `("1" riece-command-switch-to-channel-by-number-1
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
    "$" end-of-buffer
    "/" riece-command-raw
    ">" end-of-buffer
    "<" beginning-of-buffer
    "^" riece-command-list-addons
    "\C-ta" riece-command-toggle-away
    "c" riece-command-select-command-buffer
    "f" riece-command-finger
    "\C-tf" riece-command-toggle-freeze
    "\C-to" riece-command-toggle-own-freeze
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
    "x" riece-command-copy-region
    "t" riece-command-topic
    "w" riece-command-who)

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
If optional argument CONFIRM is non-nil, ask which IRC server to connect."
  (interactive "P")
  (riece-read-variables-files (if noninteractive
				  (car command-line-args-left)))
  (run-hooks 'riece-after-load-startup-hook)
  (if (riece-server-opened)
      (riece-command-configure-windows)
    (unless riece-addons-insinuated
      (setq riece-addons (riece-resolve-addons riece-addons))
      (let ((pointer riece-addons))
	(while pointer
	  (riece-insinuate-addon (car pointer) riece-debug)
	  (setq pointer (cdr pointer))))
      (setq riece-addons-insinuated t))
    (if (or confirm (null riece-server))
	(setq riece-server (completing-read "Server: " riece-server-alist)))
    (if (stringp riece-server)
	(setq riece-server (riece-server-name-to-server riece-server)))
    (riece-create-buffers)
    (if riece-max-buffer-size
	(setq riece-shrink-buffer-idle-timer
	      (riece-run-with-idle-timer
	       riece-shrink-buffer-idle-time-delay t
	       (lambda ()
		 (let ((buffers riece-buffer-list))
		   (while buffers
		     (if (eq (derived-mode-class
			      (with-current-buffer (car buffers)
				major-mode))
			     'riece-dialogue-mode)
			 (riece-shrink-buffer (car buffers)))
		     (setq buffers (cdr buffers))))))))
    (switch-to-buffer riece-command-buffer)
    (riece-display-connect-signals)
    (riece-redisplay-buffers)
    (riece-open-server riece-server "")
    (let ((server-list riece-startup-server-list))
      (while server-list
	(riece-command-open-server (car server-list))
	(setq server-list (cdr server-list))))
    (let ((pointer riece-addons))
      (while pointer
	(unless (get (car pointer) 'riece-addon-default-disabled)
	  (riece-enable-addon (car pointer) riece-debug))
	(setq pointer (cdr pointer))))
    (run-hooks 'riece-startup-hook)
    (message "%s" (substitute-command-keys
		   "Type \\[describe-mode] for help"))))

(defun riece-shrink-buffer (buffer)
  (save-excursion
    (set-buffer buffer)
    (goto-char (point-min))
    (while (> (buffer-size) riece-max-buffer-size)
      (let* ((inhibit-read-only t)
	     buffer-read-only
	     (start (point))
	     (end (progn (beginning-of-line 2) (point)))
	     (overlays (riece-overlays-in start end)))
	(while overlays
	  (riece-delete-overlay (car overlays))
	  (setq overlays (cdr overlays)))
	(delete-region start end)))))

(defun riece-exit ()
  (if riece-save-variables-are-dirty
      (riece-save-variables-files))
  (while riece-buffer-list
    (if (and (get-buffer (car riece-buffer-list))
	     (buffer-live-p (car riece-buffer-list)))
	(funcall riece-buffer-dispose-function (car riece-buffer-list)))
    (setq riece-buffer-list (cdr riece-buffer-list)))
  (if riece-shrink-buffer-idle-timer
      (riece-cancel-timer riece-shrink-buffer-idle-timer))
  (setq riece-server nil
	riece-current-channels nil
	riece-current-channel nil
	riece-channel-buffer nil
	riece-channel-buffer-alist nil
	riece-user-indicator nil
	riece-long-channel-indicator "None"
	riece-channel-list-indicator "No channel"
	riece-away-indicator "-"
	riece-operator-indicator "-"
	riece-freeze-indicator "-")
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

  (setq riece-away-indicator "-"
	riece-operator-indicator "-"
	major-mode 'riece-command-mode
	mode-name "Command"
	mode-line-buffer-identification
	(riece-mode-line-buffer-identification
	 '("Riece: "
	   riece-away-indicator
	   riece-operator-indicator
	   " "
	   riece-user-indicator
	   " "
	   riece-channel-indicator)))
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
  (make-local-variable 'riece-freeze-indicator)
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
  (setq mode-line-buffer-identification
	(riece-mode-line-buffer-identification
	 '("Riece: "
	   riece-away-indicator
	   riece-operator-indicator
	   riece-freeze-indicator
	   " "
	   riece-long-channel-indicator))))

(defun riece-channel-list-mode ()
  "Major mode for displaying channel list.
All normal editing commands are turned off."
  (kill-all-local-variables)
  (buffer-disable-undo)
  (setq major-mode 'riece-channel-list-mode
	mode-name "Channels"
	mode-line-buffer-identification
	(riece-mode-line-buffer-identification '("Riece: "))
	truncate-lines t
	buffer-read-only t)
  (make-local-hook 'riece-update-buffer-functions)
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
  (setq major-mode 'riece-user-list-mode
	mode-name "Users"
	mode-line-buffer-identification
	(riece-mode-line-buffer-identification
	 '("Riece: " riece-long-channel-indicator " "))
	truncate-lines t
	buffer-read-only t)
  (if (boundp 'transient-mark-mode)
      (set (make-local-variable 'transient-mark-mode) t))
  (make-local-hook 'riece-update-buffer-functions)
  (add-hook 'riece-update-buffer-functions
	    'riece-update-user-list-buffer nil t)
  (use-local-map riece-user-list-mode-map)
  (run-hooks 'riece-user-list-mode-hook))

(defun riece-create-buffers ()
  (let ((alist riece-buffer-alist))
    (while alist
      (save-excursion
	(set-buffer (apply #'riece-get-buffer-create
			   (cdr (car alist))))
	(set (car (car alist)) (current-buffer))
	(unless (or (null (nth 2 (car alist)))
		    (eq major-mode (nth 2 (car alist))))
	  (funcall (nth 2 (car alist))))
	(setq alist (cdr alist))))))

(provide 'riece)

;;; riece.el ends here
