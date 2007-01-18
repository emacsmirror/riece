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
  (if (and form (listp form))
      (if (eq (car form) 'riece-mcat)
	  (cdr form)
	(delq nil (apply #'nconc
			 (mapcar #'riece-mcat-extract-from-form form))))))

(defun riece-mcat-extract (files alist)
  (let (message-list pointer)
    (while files
      (save-excursion
	(set-buffer (find-file-noselect (car files)))
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
    (setq message-list (sort message-list #'string-lessp)
	  pointer message-list)
    (while pointer
      (if (equal (car pointer)
		 (nth 1 pointer))
	  (setcdr pointer (nth 2 pointer))
	(unless (assoc (car pointer) alist)
	  (setq alist (cons (list (car pointer)) alist))))
      (setq pointer (cdr pointer)))
    alist))

(provide 'riece-mcat)
