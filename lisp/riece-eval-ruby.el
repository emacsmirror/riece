(require 'riece-ruby)
(require 'riece-message)

(defvar riece-eval-ruby-enabled nil)

(defconst riece-eval-ruby-description
  "Evaluate an input string as Ruby program.")

(defun riece-eval-ruby-exit-handler (name)
  (riece-ruby-inspect name)
  (let* ((data (copy-sequence riece-ruby-data))
	 (length (length data))
	 (index 0))
    (while (< index length)
      (if (eq (aref data index) ?\n)
	  (aset data index ? ))
      (setq index (1+ index)))
    (riece-send-string
     (format "NOTICE %s :%s\r\n"
	     (riece-identity-prefix
	      (riece-ruby-property name 'riece-eval-ruby-target))
	     data))
    (riece-display-message
     (riece-make-message (riece-current-nickname)
			 (riece-ruby-property name 'riece-eval-ruby-target)
			 data
			 'notice))))

(defun riece-eval-ruby-display-message-function (message)
  (if (and riece-eval-ruby-enabled
	   (riece-message-own-p message)
	   (string-match "^,ruby\\s-+" (riece-message-text message)))
      (let ((name (riece-ruby-execute
		   (substring (riece-message-text message)
			      (match-end 0)))))
	(riece-ruby-set-property name
				 'riece-eval-ruby-target
				 (riece-message-target message))
	(riece-ruby-set-exit-handler name
				     #'riece-eval-ruby-exit-handler))))

(defun riece-eval-ruby-insinuate ()
  (add-hook 'riece-after-display-message-functions
	    'riece-eval-ruby-display-message-function))

(defun riece-eval-ruby-enable ()
  (setq riece-eval-ruby-enabled t))

(defun riece-eval-ruby-disable ()
  (setq riece-eval-ruby-enabled nil))

(provide 'riece-eval-ruby)