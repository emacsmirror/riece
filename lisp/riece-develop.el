(defun riece-insert-struct-template (prefix struct)
  (interactive "sPrefix: 
sStruct: ")
  (let (attributes
	optional-attributes
	name
	pointer
	arglist
	strings
	(index 0))
    (catch 'finish
      (while t
	(setq name (read-from-minibuffer "Attribute: "))
	(if (equal name "")
	    (throw 'finish nil))
	(setq attributes
	      (cons (vector name
			    (y-or-n-p "Optional? ")
			    (y-or-n-p "Readable? ")
			    (y-or-n-p "Writable? "))
		    attributes))))
    (setq attributes (nreverse attributes)
	  pointer (cons (vector "" nil nil nil)  attributes))
    (while (cdr pointer)
      (if (aref (car (cdr pointer)) 1)
	  (progn
	    (setq optional-attributes (cons (car (cdr pointer))
					    optional-attributes))
	    (setcdr pointer (nthcdr 2 pointer)))
	(setq pointer (cdr pointer))))
    (setq optional-attributes (nreverse optional-attributes)
	  arglist (mapconcat (lambda (attribute)
			       (aref attribute 0))
			     attributes " "))
    (if optional-attributes
	(setq arglist (concat arglist " &optional "
			      (mapconcat (lambda (attribute)
					   (aref attribute 0))
					 optional-attributes " "))))
    (setq strings (list (format "\
\(defun %smake-%s (%s)
  \"Make %s%s object.\"
  (vector %s))"
				prefix struct arglist
				prefix struct
				(mapconcat (lambda (attribute)
					     (aref attribute 0))
					   (append attributes
						   optional-attributes)
					   " "))))
    (setq pointer (append attributes optional-attributes))
    (while pointer
      (if (aref (car pointer) 2)
	  (setq strings (cons (format "\
\(defun %s%s-%s (%s)
  \"Return %s of %s.\"
  (aref %s %d))"
				      prefix struct (aref (car pointer) 0)
				      struct
				      (aref (car pointer) 0)
				      (upcase struct)
				      struct index)
			      strings)))
      (if (aref (car pointer) 3)
	  (setq strings (cons (format "\
\(defun %s%s-set-%s (%s %s)
  \"Set %s of %s to %s.\"
  (aset %s %d %s))"
				      prefix struct (aref (car pointer) 0)
				      struct (aref (car pointer) 0)
				      (aref (car pointer) 0)
				      (upcase struct)
				      (upcase (aref (car pointer) 0))
				      struct index (aref (car pointer) 0))
			      strings)))
      (setq pointer (cdr pointer)
	    index (1+ index)))
    (insert (mapconcat #'identity (nreverse strings) "\n\n"))))
