;;; url-riece.el --- Adapting `riece' to `url-irc'
;; Copyright (C) 2004 Masatake YAMATO

;; Author: Masatake YAMATO <jet@gyve.org>
;; Keywords: IRC, riece, url, comm, data, processes

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
;;
;; With this package you can opne an url which protocol is irc by
;; riece via url package of GNU Emacs.
;;
;; e.g.
;; (url-retrieve-synchronously "irc://irc.gnome.org:6667/#gtk+")
;; (url-mm-url "irc://irc.gnome.org:6667/#gtk+")
;;

;;; Code:
(require 'riece)
(require 'url)
(require 'url-irc)

(defun url-irc-riece-ready-p ()
  "Riece is active or not.
\(If it is active, a server named \"\" may exists.)"
  (and (boundp 'riece-server-process-alist)
       riece-server-process-alist))

;; Based on the code posted to liece ml by Daiki Ueno <ueno@unixuser.org>
;; Message-Id: <612cb699-83c0-47ad-a991-423c46bc8384@well-done.deisui.org>
(defun url-irc-riece-find-server (host &optional port)
  "Find an entry for HOST:PORT in `riece-server-process-alist'."
  (unless port (setq port 6667))
  (catch 'found
    (let (name name-sans-service plist)
      (mapc (lambda (pointer)
	      (setq name (car pointer)
		    name-sans-service (plist-get 
				       (riece-server-name-to-server name) 
				       :host)
		    plist (if (equal name "")
			      riece-server
			    (cdr (or (assoc name riece-server-alist)
				     (assoc name-sans-service riece-server-alist)))))
	      (when (and plist
			 (equal (plist-get plist :host) host)
			 (eq (or (plist-get plist :service) 6667) port))
		(throw 'found pointer)))
	    riece-server-process-alist)
      nil)))
;(url-irc-riece-find-server "localhost")
;(url-irc-riece-find-server "localhost" 6667)
;(url-irc-riece-find-server "irc.gnome.org")

(defun url-irc-riece (host port channel user password)
  "Adapting `riece' to `url-irc'.
See the documentation of `url-irc-function'about HOST, PORT, CHANNEL, USER
and PASSWORD. Just give nil to it."
  (unless user (setq user riece-nickname))
  (let ((server (if port (format "%s:%d" host port) host)))
    (cond
     ((not (url-irc-riece-ready-p))
      (setq riece-server server)
      (let ((riece-default-password password)
	    (riece-nickname user))
	;; Just start riece
	(riece))
      (url-irc-riece host port channel user password))
     ((not (url-irc-riece-find-server host port))
      (let ((riece-default-password password)
	    (riece-nickname user))
	;; Just open the server
	(riece-command-open-server server))
      (url-irc-riece host port channel user password))
     (t
      (let ((server-name (car (url-irc-riece-find-server host port))))
	(riece-command-join 
	 (riece-parse-identity (if (string= server-name "")
				   channel
				 (format "%s %s" channel server-name)))))
      ;; Show the windows
      (riece)))))
; (url-irc-riece "localhost" nil "#mandara" "jetgx" nil)
; (url-irc-riece "localhost" nil "#misc" "jetgx" nil)
; (url-irc-riece "irc.gnome.org" nil "#mandara" "jetgx" nil)
; (url-irc-riece "irc.gnome.org" nil "#misc" "jetgx" nil)

(setq url-irc-function 'url-irc-riece)

(provide 'url-riece)

;; arch-tag: b54bcdf0-0ee3-447b-bc07-e7329d9f2f45
;;; url-riece.el ends here
