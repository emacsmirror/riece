(require 'riece-log)

(luna-define-class test-riece-log (lunit-test-case))

(defun test-riece-log-delete-directory (directory)
  (let ((files (directory-files directory t nil t)))
    (while files
      (if (file-directory-p (car files))
	  (unless (member (file-name-nondirectory (car files)) '("." ".."))
	    (test-riece-log-delete-directory (car files)))
	(delete-file (car files)))
      (setq files (cdr files)))
    (delete-directory directory)))

(luna-define-method test-riece-log-flashback ((case test-riece-log))
  (let ((riece-log-directory
	 (expand-file-name (make-temp-name "test-riece-log")
			   (if (featurep 'xemacs)
			       (temp-directory)
			     temporary-file-directory)))
	riece-addons)
    (unwind-protect
	(let ((dir (expand-file-name "riece" riece-log-directory))
	      (riece-log-flashback 2)
	      riece-log-directory-map)
	  (make-directory riece-log-directory)
	  (make-directory dir)
	  (write-region "03:14 <test> a b c\n" nil
			(expand-file-name "20380119.log" dir) t 0)
	  (write-region "03:14 <test> 1 2 3\n" nil
			(expand-file-name "20380118.log" dir) t 0)
	  (lunit-assert-2
	   case
	   (equal
	    "03:14 <test> 1 2 3 (2038/01/18)\n03:14 <test> a b c (2038/01/19)\n"
	    (with-temp-buffer
	      (riece-log-flashback (riece-make-identity "#riece" ""))
	      (buffer-string)))))
      (test-riece-log-delete-directory riece-log-directory))))
    
