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

(luna-define-method lunit-test-case-setup ((case test-riece-log))
  (setq riece-log-directory
	(expand-file-name (make-temp-name "test-riece-log")
			   (if (featurep 'xemacs)
			       (temp-directory)
			     temporary-file-directory))
	test-riece-log-directory
	(expand-file-name "riece" riece-log-directory))
  (make-directory riece-log-directory)
  (make-directory test-riece-log-directory)
  (write-region "03:14 <test> a b c\n03:15 <test> a b c\n" nil
		(expand-file-name "20380119.log" test-riece-log-directory)
		t 0)
  (write-region "03:14 <test> 1 2 3\n" nil
		(expand-file-name "20380118.log" test-riece-log-directory)
		t 0))

(luna-define-method lunit-test-case-teardown ((case test-riece-log))
  (test-riece-log-delete-directory riece-log-directory))

(luna-define-method test-riece-log-flashback-1 ((case test-riece-log))
  (let ((riece-log-flashback 3)
	riece-log-directory-map)
    (lunit-assert-2
     case
     (equal
      (concat "03:14 <test> 1 2 3 (2038/01/18)\n"
	      "03:14 <test> a b c (2038/01/19)\n"
	      "03:15 <test> a b c (2038/01/19)\n")
      (with-temp-buffer
	(riece-log-flashback (riece-make-identity "#riece" ""))
	(buffer-string))))))

(luna-define-method test-riece-log-flashback-2 ((case test-riece-log))
  (let ((riece-log-flashback t)
	riece-log-directory-map)
    (lunit-assert-2
     case
     (equal ""
	    (with-temp-buffer
	      (riece-log-flashback (riece-make-identity "#riece" ""))
	      (buffer-string))))))
