;;; riece-rdcc.el --- ruby implementation of DCC add-on
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

(require 'riece-globals)
(require 'riece-misc)
(require 'riece-channel)
(require 'riece-identity)
(require 'riece-ctcp)			;for riece-ctcp-additional-clientinfo

(defgroup riece-rdcc nil
  "DCC implementation using ruby"
  :prefix "riece-"
  :group 'riece)

(defcustom riece-rdcc-server-address nil
  "Local address of the DCC server.
Only used for sending files."
  :type 'string
  :group 'riece-rdcc)

(defcustom riece-rdcc-ruby-command "ruby"
  "Command name for Ruby interpreter."
  :type 'string
  :group 'riece-rdcc)

(defcustom riece-rdcc-send-program
  '("\
address = " address "
unless address
  sock = UDPSocket.new
  sock.connect('164.46.176.4', 7)		# www.unixuser.org/echo
  address = sock.getsockname[4 .. 7].unpack('CCCC').join('.')
end
server = TCPServer.new(address, 0)
puts(\"#{server.addr[3].split(/\\./).collect{|c| c.to_i}.pack('CCCC').unpack('N')[0]} #{server.addr[1]}\")
$stdout.flush
session = server.accept
if session
  total = 0
  File.open(" file ") {|file|
    while (bytes = file.read(" block-size "))
      total += bytes.length
      puts(\"#{total}\")
      session.write(bytes)
    end
  }
  session.close
end
")
  "Ruby program to send file with DCC."
  :type 'list
  :group 'riece-rdcc)

(defcustom riece-rdcc-decode-address-program
  '("\
puts(\"#{" address " >> 24 & 0xFF}.#{" address " >> 16 & 0xFF}.#{"
    address " >> 8 & 0xFF}.#{" address " & 0xFF}\")")
  "Ruby program to numeric IP address."
  :type 'list
  :group 'riece-rdcc)

(defcustom riece-rdcc-save-directory nil
  "Default directory where received files are saved in."
  :type 'directory
  :group 'riece-rdcc)

(defcustom riece-rdcc-block-size 4096
  "Number of bytes sent as a block."
  :type 'integer
  :group 'riece-rdcc)

(defvar riece-rdcc-requests nil)

(defvar riece-rdcc-request-user nil)
(defvar riece-rdcc-request-file nil)
(defvar riece-rdcc-request-size nil)

(defvar riece-rdcc-temp-file nil)
(defvar riece-rdcc-received-size nil)

(defvar temporary-file-directory)
(defvar jka-compr-compression-info-list)
(defvar jam-zcat-filename-list)
(defun riece-rdcc-substitute-variables (program variable value)
  (setq program (copy-sequence program))
  (let ((pointer program))
    (while pointer
      (setq pointer (memq variable program))
      (if pointer
	  (setcar pointer value)))
    program))

(defun riece-rdcc-server-filter (process input)
  (save-excursion
    (set-buffer (process-buffer process))
    (goto-char (point-max))
    (insert input)
    (goto-char (point-min))
    (while (and (not (eobp))
		(looking-at "\\([0-9]+\\)\n"))
      (message "Sending %s...(%s/%d)"
	       riece-rdcc-request-file
	       (match-string 1) riece-rdcc-request-size)
      (forward-line))
    (unless (eobp)
      (delete-region (point-min) (point)))))

(defun riece-rdcc-server-sentinel (process status)
  (with-current-buffer (process-buffer process)
    (message "Sending %s...done" riece-rdcc-request-file))
  (kill-buffer (process-buffer process)))

(defun riece-command-dcc-send (user file)
  (interactive
   (let ((completion-ignore-case t))
     (list (riece-completing-read-identity
	    "User: "
	    (riece-get-users-on-server (riece-current-server-name)))
	   (expand-file-name (read-file-name "File: ")))))
  (let* ((process-connection-type nil)
	 (process (start-process "DCC" (generate-new-buffer " *DCC*")
				 "ruby" "-rsocket")))
    (process-send-string process
			 (apply #'concat
				(riece-rdcc-substitute-variables
				 (riece-rdcc-substitute-variables
				  (riece-rdcc-substitute-variables
				   riece-rdcc-send-program
				   'address
				   (if riece-rdcc-server-address
				       (concat "'" riece-rdcc-server-address
					       "'")
				     "nil"))
				  'file
				  (concat "'" file "'"))
				 'block-size
				 (number-to-string riece-rdcc-block-size))))
    (process-send-eof process)
    (save-excursion
      (set-buffer (process-buffer process))
      (while (and (eq (process-status process) 'run)
		  (progn
		    (goto-char (point-min))
		    (not (looking-at "\\([0-9]+\\) \\([0-9]+\\)"))))
	(accept-process-output process))
      (if (eq (process-status process) 'run)
	  (let ((address (match-string 1))
		(port (match-string 2)))
	    (erase-buffer)
	    (make-local-variable 'riece-rdcc-request-size)
	    (setq riece-rdcc-request-file file
		  riece-rdcc-request-size (nth 7 (file-attributes file)))
	    (set-buffer-modified-p nil)
	    (set-process-filter process #'riece-rdcc-server-filter)
	    (set-process-sentinel process #'riece-rdcc-server-sentinel)
	    (riece-send-string
	     (format "PRIVMSG %s :\1DCC SEND %s %s %s %d\1\r\n"
		     (riece-identity-prefix user)
		     (file-name-nondirectory file)
		     address port
		     riece-rdcc-request-size)))))))

(defun riece-rdcc-filter (process input)
  (save-excursion
    (set-buffer (process-buffer process))
    (erase-buffer)
    (insert input)
    (let ((coding-system-for-write 'binary)
	  jka-compr-compression-info-list jam-zcat-filename-list)
      (write-region (point-min) (point-max) riece-rdcc-temp-file t 0))
    (message "Receiving %s from %s...(%s/%s)"
	     (file-name-nondirectory riece-rdcc-request-file)
	     riece-rdcc-request-user
	     (riece-rdcc-format-size
	      (setq riece-rdcc-received-size (+ (buffer-size)
						riece-rdcc-received-size)))
	     (riece-rdcc-format-size riece-rdcc-request-size))))

(defun riece-rdcc-sentinel (process status)
  (save-excursion
    (set-buffer (process-buffer process))
    (unless (= riece-rdcc-received-size riece-rdcc-request-size)
      (error "Premature end of file"))
    (message "Receiving %s from %s...done"
	     (file-name-nondirectory riece-rdcc-request-file)
	     riece-rdcc-request-user)
    (condition-case nil
	(progn
	  (rename-file riece-rdcc-temp-file riece-rdcc-request-file)
	  (delete-directory (file-name-directory riece-rdcc-temp-file)))
      (file-already-exists
       (error "Can't save %s.  Temporarily saved in %s"
	      riece-rdcc-request-file riece-rdcc-temp-file))))
  (kill-buffer (process-buffer process)))

(defun riece-rdcc-decode-address (address)
  (with-temp-buffer
    (call-process riece-rdcc-ruby-command nil t nil "-e"
		  (apply #'concat
			 (riece-rdcc-substitute-variables
			  riece-rdcc-decode-address-program
			  'address
			  address)))
    (buffer-substring (point-min) (1- (point-max)))))

(defun riece-command-dcc-receive (request file)
  (interactive
   (progn
     (unless riece-rdcc-requests
       (error "No request"))
     (let* ((request
	     (if (= (length riece-rdcc-requests) 1)
		 (car riece-rdcc-requests)
	       (with-output-to-temp-buffer "*Help*"
		 (let ((requests riece-rdcc-requests)
		       (index 1))
		   (while requests
		     (princ (format "%2d: %s %s (%d bytes)\n"
				    index
				    (car (car requests))
				    (nth 1 (car requests))
				    (nth 4 (car requests))))
		     (setq index (1+ index)
			   requests (cdr requests)))))
	       (let ((number (read-string "Request#: ")))
		 (unless (string-match "^[0-9]+$" number)
		   (error "Not a number"))
		 (if (or (> (setq number (string-to-number number))
			    (length riece-rdcc-requests))
			 (< number 1))
		     (error "Invalid number"))
		 (nth (1- number) riece-rdcc-requests))))
	    (default-name (expand-file-name
			   (nth 1 request) (or riece-rdcc-save-directory
					       default-directory))))
       (list request
	     (expand-file-name
	      (read-file-name
	       (concat "Save as (default "
		       (file-name-nondirectory default-name) ") ")
	       (file-name-directory default-name)
	       default-name))))))
  (let* ((temp-file (expand-file-name
		     (file-name-nondirectory file)
		     (expand-file-name (make-temp-name "riece-rdcc")
				       (if (featurep 'xemacs)
					   (temp-directory)
					 temporary-file-directory))))
	 (orig-mode (default-file-modes))
	 selective-display
	 (coding-system-for-read 'binary)
	 (coding-system-for-write 'binary)
	 process)
    (unwind-protect
	(progn
	  (set-default-file-modes 448)
	  ;; This may throw an error.
	  (make-directory (file-name-directory temp-file)))
      (set-default-file-modes orig-mode))
    (setq process (open-network-stream
		   "DCC" (generate-new-buffer " *DCC*")
		   (riece-rdcc-decode-address (nth 2 request))
		   (nth 3 request)))
    (setq riece-rdcc-requests (delq request riece-rdcc-requests))
    (with-current-buffer (process-buffer process)
      (if (fboundp 'set-buffer-multibyte)
	  (set-buffer-multibyte nil))
      (buffer-disable-undo)
      (erase-buffer)
      (make-local-variable 'riece-rdcc-request-user)
      (setq riece-rdcc-request-user (car request))
      (make-local-variable 'riece-rdcc-request-file)
      (setq riece-rdcc-request-file file)
      (make-local-variable 'riece-rdcc-request-size)
      (setq riece-rdcc-request-size (nth 4 request))
      (make-local-variable 'riece-rdcc-temp-file)
      (setq riece-rdcc-temp-file temp-file)
      (make-local-variable 'riece-rdcc-received-size)
      (setq riece-rdcc-received-size 0))
    (set-process-filter process #'riece-rdcc-filter)
    (set-process-sentinel process #'riece-rdcc-sentinel)))

(defun riece-rdcc-format-size (size)
  (if (< size 1024)
      (format "%0.1f" size)
    (setq size (/ size 1024.0))
    (if (< size 1024)
	(format "%0.1fKB" size)
      (setq size (/ size 1024.0))
      (if (< size 1024)
	  (format "%0.1fMB" size)
	(format "%0.1fGB" (/ size 1024.0))))))

(defun riece-handle-dcc-request (prefix target message)
  (let ((case-fold-search t))
    (when (string-match
	   "SEND \\([^ ]+\\) \\([^ ]+\\) \\([^ ]+\\) \\([^ ]+\\)"
	   message)
      (let ((file (match-string 1 message))
	    (address (match-string 2 message))
	    (port (string-to-number (match-string 3 message)))
	    (size (string-to-number (match-string 4 message)))
	    (buffer (if (riece-channel-p target)
			(riece-channel-buffer (riece-make-identity
					       target riece-server-name))))
	    (user (riece-prefix-nickname prefix)))
	(setq riece-rdcc-requests
	      (cons (list user file address port size)
		    riece-rdcc-requests))
	(message "%s"
		 (with-current-buffer (window-buffer (selected-window))
		   (substitute-command-keys
		    (format
		     "Type \\[riece-command-dcc-receive] to receive"
		     user))))
	(riece-insert-change buffer (format "DCC SEND from %s: %s (%s)\n"
					    user file
					    (riece-rdcc-format-size size)))
	(riece-insert-change
	 (if (and riece-channel-buffer-mode
		  (not (eq buffer riece-channel-buffer)))
	     (list riece-dialogue-buffer riece-others-buffer)
	   riece-dialogue-buffer)
	 (concat
	  (riece-concat-server-name
	   (format "DCC SEND from %s (%s) to %s: %s (%s)"
		   user
		   (riece-strip-user-at-host
		    (riece-prefix-user-at-host prefix))
		   (riece-decode-coding-string target)
		   file
		   (riece-rdcc-format-size size)))
	  "\n")))
      t)))

(defun riece-rdcc-requires ()
  '(riece-ctcp))

(defvar riece-dialogue-mode-map)
(defun riece-rdcc-insinuate ()
  (add-to-list 'riece-ctcp-additional-clientinfo "DCC")
  (add-hook 'riece-ctcp-dcc-request-hook 'riece-handle-dcc-request)
  (define-key riece-dialogue-mode-map "\C-ds" 'riece-command-dcc-send)
  (define-key riece-dialogue-mode-map "\C-dr" 'riece-command-dcc-receive))

(provide 'riece-rdcc)

;;; riece-rdcc.el ends here
