(defvar riece-mcat-alist
  '(("Japanese" . riece-mcat-japanese)))

(defun riece-mcat (string)
  (let ((entry (assoc current-language-environment riece-mcat-alist)))
    (when entry
      (require (cdr entry))
      (or (cdr (assoc string (symbol-value (intern
					    (concat (symbol-name (cdr entry))
						    "-alist")))))
	  string))))

(defun riece-mcat-extract-from-form (form)
  (if (and form (listp form) (listp (cdr form)))
      (if (eq (car form) 'riece-mcat)
	  (cdr form)
	(delq nil (apply #'nconc
			 (mapcar #'riece-mcat-extract-from-form form))))))

(defun riece-mcat-extract (files alist)
  (save-excursion
    (let (message-list)
      (while files
	(with-temp-buffer
	  (insert-file-contents (car files))
	  (goto-char (point-min))
	  (while (progn
		   (while (progn (skip-chars-forward " \t\n\f")
				 (looking-at ";"))
		     (forward-line 1))
		   (not (eobp)))
	    (setq message-list
		  (nconc message-list
			 (riece-mcat-extract-from-form
			  (read (current-buffer)))))))
	(setq files (cdr files)))
      (setq message-list (sort message-list #'string-lessp))
      (while message-list
	(if (equal (car message-list)
		   (nth 1 message-list))
	    (setq message-list (nthcdr 2 message-list))
	  (unless (assoc (car message-list) alist)
	    (setq alist (cons (list (car message-list)) alist)))
	  (setq message-list (cdr message-list))))
      alist)))

(defun riece-mcat-update (files mcat-file mcat-alist)
  (let (alist)
    (save-excursion
      (set-buffer (find-file-noselect mcat-file))
      (goto-char (point-min))
      (re-search-forward (concat "^\\s-*(\\(defvar\\|defconst\\)\\s-+"
				 (regexp-quote (symbol-name mcat-alist))))
      (goto-char (match-beginning 0))
      (save-excursion
	(eval (read (current-buffer))))
      (setq alist (riece-mcat-extract files (symbol-value mcat-alist)))
      (delete-region (point) (progn (forward-sexp) (point)))
      (insert "(defconst " (symbol-name mcat-alist) "\n  '(")
      (while alist
	(insert "(" (prin1-to-string (car (car alist))) " . "
		(prin1-to-string (cdr (car alist))) ")")
	(if (cdr alist)
	    (insert "\n   "))
	(setq alist (cdr alist)))
      (insert "))")
      (save-buffer))))

(provide 'riece-mcat)
