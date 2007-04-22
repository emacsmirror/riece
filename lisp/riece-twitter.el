(require 'riece-message)

(defvar riece-twitter-credential nil)

(defun riece-twitter-message-filter (message)
  (if (and (riece-message-own-p message)
	   (eq 'action (riece-message-type message)))
      (start-process
       "curl" nil "curl"
       "-H" "X-Twitter-Client: Riece"
       "-H" (concat "X-Twitter-Client-Version: " riece-version-number)
       "-H" "X-Twitter-Client-URL: http://riece.nongnu.org/twitter.xml"
       "-u" riece-twitter-credential
       "-d" (concat "status="
		    (riece-twitter-escape-string (encode-coding-string (riece-message-text message) 'utf-8)))
       "-s"
       "http://twitter.com/statuses/update.json"))
  message)

(defun riece-twitter-escape-string (string)
  (let ((index 0))
    (while (string-match "[^0-9A-Za-z\-\._~:/?@!\$'()*,]" string index)
      (setq string (replace-match
		    (format "%%%02X" (aref string (match-beginning 0)))
		    t t string)
	    index (+ 3 (match-beginning 0))))
    string))

(defun riece-twitter-insinuate ()
  (add-hook 'riece-message-filter-functions 'riece-twitter-message-filter))

(defun riece-twitter-uninstall ()
  (remove-hook 'riece-message-filter-functions 'riece-twitter-message-filter))

(provide 'riece-twitter)

;;; riece-twitter.el ends here
