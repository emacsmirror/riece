;;; riece-async.el --- connect to IRC server via async proxy
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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; NOTE: This is an add-on module for Riece.

;; This program allows to connect to an IRC server via local proxy
;; which responds to PING requests from server.

;; If you want to enable this feature per server, write the server
;; spec like this:
;; (add-to-list 'riece-server-alist
;;              '("async" :host "irc.tokyo.wide.ad.jp"
;;                :function riece-async-open-network-stream))

;;; Code:

(require 'riece-options)
(require 'riece-ruby)			;riece-ruby-command

(defgroup riece-async nil
  "Connect to IRC server via async proxy."
  :prefix "riece-"
  :group 'riece)

(defcustom riece-async-buffer-size 65535
  "Maximum size of the write buffer."
  :type 'integer
  :group 'riece-async)

(defcustom riece-async-backup-file (expand-file-name "riece-async.bak"
						     riece-directory)
  "A file which contains outdated messages."
  :type 'string
  :group 'riece-async)

(defvar riece-async-server-program "aproxy.rb"
  "The server program file.  If the filename is not absolute, it is
assumed that the file is in the same directory of this file.")

(defvar riece-async-server-program-arguments
  (list "-s" (number-to-string riece-async-buffer-size)
	"-b" riece-async-backup-file)
  "Command line arguments passed to `riece-async-server-program'.")

(defconst riece-async-description
  "Connect to IRC server via async proxy.")

;;;###autoload
(defun riece-async-open-network-stream (name buffer host service)
  (let* (process-connection-type
	 (process
	  (apply #'start-process name buffer riece-ruby-command
		 (expand-file-name riece-async-server-program
				   riece-data-directory)
		 riece-async-server-program-arguments)))
    (if buffer
	(with-current-buffer (process-buffer process)
	  (while (and (eq (process-status process) 'run)
		      (progn
			(goto-char (point-min))
			(not (looking-at (format "NOTICE CONNECTED %d"
						 (process-id process))))))
	    (accept-process-output process))))
    (riece-set-process-query-on-exit-flag process nil)
    process))

(defun riece-async-insinuate ()
  (setq riece-default-open-connection-function
	#'riece-async-open-network-stream))

(provide 'riece-async)

;;; riece-async.el ends here
