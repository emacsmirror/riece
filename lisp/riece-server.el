;;; riece-server.el --- functions to open and close servers
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

(require 'riece-options)
(require 'riece-globals)		;for server local variables.
(require 'riece-coding)			;riece-default-coding-system
(require 'riece-identity)
(require 'riece-compat)

(eval-and-compile
  (defvar riece-server-keyword-map
    '((:host)
      (:service 6667)
      (:nickname riece-nickname)
      (:username riece-username)
      (:password)
      (:function riece-default-open-connection-function)
      (:coding riece-default-coding-system))
    "Mapping from keywords to default values.
All keywords that can be used must be listed here."))

(defmacro riece-server-keyword-bind (plist &rest body)
  "Return a `let' form that binds all variables in PLIST.
After this is done, BODY will be executed in the scope
of the `let' form.

The variables bound and their default values are described by
the `riece-server-keyword-map' variable."
  `(let ,(mapcar
	  (lambda (keyword)
	    (list (intern (substring (symbol-name (car keyword)) 1))
		  (if (cadr keyword)
		      `(or (plist-get ,plist ',(car keyword))
			   ,(cadr keyword))
		    `(plist-get ,plist ',(car keyword)))))
	  riece-server-keyword-map)
     ,@body))

(put 'riece-server-keyword-bind 'lisp-indent-function 1)
(put 'riece-server-keyword-bind 'edebug-form-spec '(form body))

(defun riece-server-parse-string (string)
  "Convert a STRING set as `riece-server' and return a property list."
  (when (or (string-match "^\\[\\([^]]+\\)\\]:?\\([0-9]*\\)" string)
	    (string-match "^\\([^:]+\\):?\\([0-9]*\\)" string))
    (let ((host (match-string 1 string))
	  (service (match-string 2 string))
	  (password (substring string (match-end 0)))
	  plist)
      (setq plist (cons `(:host ,host) plist))
      (unless (equal service "")
	(setq plist (cons `(:service ,(string-to-int service)) plist)))
      (unless (equal password "")
	(setq plist (cons `(:password ,(substring password 1)) plist)))
      (apply #'nconc plist))))

(defun riece-server-name-to-server (server-name)
  (let ((entry (assoc server-name riece-server-alist)))
    (if entry
	(unless (listp (cdr entry))
	  (setcdr entry (riece-server-parse-string (cdr entry))))
      (setq entry (cons server-name (riece-server-parse-string server-name))
	    riece-server-alist (cons entry riece-server-alist)
	    riece-save-variables-are-dirty t))
    (cdr entry)))

(defun riece-server-process-name (server-name)
  (if (equal server-name "")
      "IRC"
    (format "IRC<%s>" server-name)))

(defun riece-server-process (server-name)
  (cdr (assoc server-name riece-server-process-alist)))

(defmacro riece-with-server-buffer (server-name &rest body)
  `(let ((process (riece-server-process ,server-name)))
     (if process
	 (with-current-buffer (process-buffer process)
	   ,@body)
       (error "Server closed"))))

(put 'riece-with-server-buffer 'lisp-indent-function 1)
(put 'riece-with-server-buffer 'edebug-form-spec '(form body))

(defun riece-flush-send-queue (process &optional reset)
  (with-current-buffer (process-buffer process)
    (let ((length 0)
	  string)
      (if reset
	  (setq riece-send-size 0))
      (while (and riece-send-queue
		  (<= riece-send-size riece-max-send-size))
	(setq string (riece-encode-coding-string (car riece-send-queue))
	      length (length string))
	(if (> length riece-max-send-size)
	    (message "Long message (%d > %d)" length riece-max-send-size)
	  (setq riece-send-size (+ riece-send-size length))
	  (if (<= riece-send-size riece-max-send-size)
	      (process-send-string process string)))
	(setq riece-send-queue (cdr riece-send-queue)))
      (if riece-send-queue
	  (riece-run-at-time riece-send-delay nil
			     #'riece-flush-send-queue process t)))))

(defun riece-process-send-string (process string)
  (with-current-buffer (process-buffer process)
    (setq riece-send-queue (nconc riece-send-queue (list string))))
  (riece-flush-send-queue process nil))

(defun riece-current-server-name ()
  (or riece-overriding-server-name
					;already in the server buffer
      (if (local-variable-p 'riece-server-name (current-buffer))
	  riece-server-name
	(if riece-current-channel
	    (riece-identity-server riece-current-channel)
	  (if (riece-server-opened "")
	      "")))))

(defun riece-send-string (string)
  (let* ((server-name (riece-current-server-name))
	 (process (riece-server-process server-name)))
    (unless process
      (error "%s" (substitute-command-keys
		   "Type \\[riece-command-open-server] to open server.")))
    (riece-process-send-string process string)))

(defun riece-open-server (server server-name)
  (let ((protocol (or (plist-get server :protocol)
		      riece-protocol))
	function
	process)
    (condition-case nil
	(require (intern (concat "riece-" (symbol-name protocol))))
      (error))
    (setq function (intern-soft (concat "riece-"
					(symbol-name protocol)
					"-open-server")))
    (unless function
      (error "\"%S\" is not supported" protocol))
    (condition-case nil
	(setq process (funcall function server server-name))
      (error))
    (when process
      (with-current-buffer (process-buffer process)
	(make-local-variable 'riece-protocol)
	(setq riece-protocol protocol))
      (setq riece-server-process-alist
	    (cons (cons server-name process)
		  riece-server-process-alist)))))

(defun riece-quit-server-process (process &optional message)
  (let ((function (intern-soft
		   (concat "riece-"
			   (with-current-buffer (process-buffer process)
			     (symbol-name riece-protocol))
			   "-quit-server-process"))))
    (if function
	(funcall function process message))))

(defun riece-reset-process-buffer (process)
  (save-excursion
    (set-buffer (process-buffer process))
    (if (fboundp 'set-buffer-multibyte)
	(set-buffer-multibyte nil))
    (kill-all-local-variables)
    (make-local-variable 'riece-real-nickname)
    (make-local-variable 'riece-last-nickname)
    (make-local-variable 'riece-nick-accepted)
    (make-local-variable 'riece-real-server-name)
    (make-local-variable 'riece-real-userhost)
    (make-local-variable 'riece-user-at-host)
    (make-local-variable 'riece-user-at-host-type)
    (make-local-variable 'riece-supported-user-modes)
    (make-local-variable 'riece-supported-channel-modes)
    (make-local-variable 'riece-channel-filter)
    (make-local-variable 'riece-server-name)
    (make-local-variable 'riece-read-point)
    (make-local-variable 'riece-send-queue)
    (make-local-variable 'riece-send-size)
    (setq riece-send-size 0)
    (setq riece-read-point (point-min))
    (make-local-variable 'riece-obarray)
    (setq riece-obarray (make-vector riece-obarray-size 0))
    (make-local-variable 'riece-coding-system)
    (buffer-disable-undo)
    (erase-buffer)))

(defun riece-close-server-process (process)
  (kill-buffer (process-buffer process))
  (setq riece-server-process-alist
	(delq (rassq process riece-server-process-alist)
	      riece-server-process-alist)))

(defun riece-server-process-opened (process)
  (not (null (memq (process-status process) '(open run)))))

(defun riece-server-opened (&optional server-name)
  (if server-name
      (let ((process (riece-server-process server-name)))
	(and process
	     (riece-server-process-opened process)))
    (let ((alist riece-server-process-alist))
      (catch 'found
	(while alist
	  (if (riece-server-process-opened (cdr (car alist)))
	      (throw 'found t))
	  (setq alist (cdr alist)))))))

(provide 'riece-server)

;;; riece-server.el ends here
