;;; riece-async.el --- connect to IRC server via asynchronous proxy
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

;;; Commentary:

;; This program allows to connect to an IRC server via local proxy
;; which responds to PING requests from server.

;; To use, add the following line to your ~/.riece/init.el:
;; (add-to-list 'riece-addons 'riece-async)

;; If you want to enable this feature per server, write the server
;; spec like this:
;; (add-to-list 'riece-server-alist
;;              '("async" :host "irc.tokyo.wide.ad.jp"
;;                :function riece-async-open-network-stream))

;;; Code:

(defgroup riece-async nil
  "Connect to IRC server via asynchronous proxy"
  :prefix "riece-"
  :group 'riece)

(defcustom riece-async-ruby-command "ruby"
  "Command name for Ruby interpreter."
  :type 'string
  :group 'riece-async)

(defcustom riece-async-server-program
  '("\
require 'io/nonblock'
socket = TCPSocket.new(" host ", " service ")
$stdout.write(\"NOTICE CONNECTED #{$$}\\r\\n\")
$stdout.flush
$stdout.nonblock = true
trap('STOP', 'IGNORE')
trap('TSTP', 'IGNORE')
wfds_in = []
buf = ''
loop do
  rfds, wfds, = select([socket, $stdin], wfds_in)
  unless wfds.empty?
    until buf.length <= " max-buffer-size "
      i = buf.index(\"\\r\\n\")
      break unless i
      buf.slice!(0 .. i + 1)
    end
    begin
      until buf.empty?
        len = $stdout.syswrite(buf)
        buf.slice!(0 .. len)
      end
      wfds_in = []
    rescue Errno::EAGAIN
    end
  end
  if rfds.include?(socket)
    line = socket.gets(\"\\r\\n\")
    break unless line
    if line =~ /^(?::[^ ]+ +)?PING +(.+)\\r\\n/i
      socket.write(\"PONG #{$1}\\r\\n\")
      socket.flush
    else
      wfds_in = [$stdout]
      buf << line
    end
  end
  if rfds.include?($stdin)
    line = $stdin.gets(\"\\r\\n\")
    break unless line
    socket.write(line)
    socket.flush
  end
end
socket.close
")
  "Ruby program of asynchronous proxy."
  :type 'list
  :group 'riece-async)

(defcustom riece-async-max-buffer-size 65535
  "Maximum size of the write buffer."
  :type 'integer
  :group 'riece-async)

(defun riece-async-substitute-variables (program variable value)
  (setq program (copy-sequence program))
  (let ((pointer program))
    (while pointer
      (setq pointer (memq variable program))
      (if pointer
	  (setcar pointer value)))
    program))

;;;###autoload
(defun riece-async-open-network-stream (name buffer host service)
  (let* ((process-connection-type nil)
	 (process (start-process name buffer "ruby" "-rsocket")))
    (process-kill-without-query process)
    (process-send-string process
			 (apply #'concat
				(riece-async-substitute-variables
				 (riece-async-substitute-variables
				  (riece-async-substitute-variables
				   riece-async-server-program
				   'host
				   (concat "'" host "'"))
				  'service
				  (if (numberp service)
				      (number-to-string service)
				    (concat "'" service "'")))
				 'max-buffer-size
				 (number-to-string
				  riece-async-max-buffer-size))))
    (process-send-string process "\0\n") ;input to process is needed
    (if buffer
	(save-excursion
	  (set-buffer (process-buffer process))
	  (while (and (eq (process-status process) 'run)
		      (progn
			(goto-char (point-min))
			(not (looking-at (format "NOTICE CONNECTED %d"
						 (process-id process))))))
	    (accept-process-output process))))
    process))

(defun riece-async-insinuate ()
  (setq riece-default-open-connection-function
	#'riece-async-open-network-stream))

(provide 'riece-async)

;;; riece-rdcc.el ends here
