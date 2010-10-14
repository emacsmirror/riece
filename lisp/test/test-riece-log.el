(require 'riece-log)

(luna-define-class test-riece-log (lunit-test-case))

(defun test-riece-log-delete-directory (directory)
  (let ((file-name-coding-system 'no-conversion)
	 (files (directory-files directory t nil t)))
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
	(expand-file-name "=23riece" riece-log-directory))
  (make-directory riece-log-directory)
  (make-directory test-riece-log-directory)
  (write-region "03:14 <test> a b c\n03:15 <test> a b c\n" nil
		(expand-file-name "19700102.txt" test-riece-log-directory)
		t 0)
  (write-region "03:14 <test> 1 2 3\n" nil
		(expand-file-name "19700101.txt" test-riece-log-directory)
		t 0))

(luna-define-method lunit-test-case-teardown ((case test-riece-log))
  (test-riece-log-delete-directory riece-log-directory))

(luna-define-method test-riece-log-insert-1 ((case test-riece-log))
  (let (riece-log-directory-map)
    (lunit-assert-2
     case
     (equal
      (concat "03:14 <test> 1 2 3 (1970/01/01)\n"
	      "03:14 <test> a b c (1970/01/02)\n"
	      "03:15 <test> a b c (1970/01/02)\n")
      (with-temp-buffer
	(riece-log-insert (riece-make-identity "#riece" "") 3)
	(buffer-string))))))

(luna-define-method test-riece-log-insert-2 ((case test-riece-log))
  (let (riece-log-directory-map)
    (lunit-assert-2
     case
     (equal ""
	    (with-temp-buffer
	      (riece-log-insert (riece-make-identity "#riece" "") t)
	      (buffer-string))))))

(luna-define-method test-riece-log-encode-file-name ((case test-riece-log))
  (let (riece-log-file-name-coding-system)
    (lunit-assert-2
     case
     (equal "=23riece"
	    (riece-log-encode-file-name "#riece")))
    (lunit-assert-2
     case
     (equal "=23riece=3A=2A=2Ejp"
	    (riece-log-encode-file-name "#riece:*.jp")))
    (lunit-assert-2
     case
     (equal "=23riece=="
	    (riece-log-encode-file-name "#riece=")))))

(luna-define-method test-riece-log-encode-file-name-mule
  ((case test-riece-log))
  (let ((riece-log-file-name-coding-system 'iso-8859-1))
    (lunit-assert-2
     case
     (equal "=23\xABriece\xBB"
	    (riece-log-encode-file-name
	     (format "#%criece%c"
		     (make-char 'latin-iso8859-1 43)
		     (make-char 'latin-iso8859-1 59))))))
  (let ((riece-log-file-name-coding-system 'iso-2022-jp))
    (lunit-assert-2
     case
     (equal "=23=1B=24B=24j=21=3C=249=1B=28B"
	    (riece-log-encode-file-name
	     (format "#%c%c%c"
		    (make-char 'japanese-jisx0208 36 106)
		    (make-char 'japanese-jisx0208 33 60)
		    (make-char 'japanese-jisx0208 36 57)))))))

(luna-define-method test-riece-log-decode-file-name ((case test-riece-log))
  (let (riece-log-file-name-coding-system)
    (lunit-assert-2
     case
     (equal "#riece"
	    (riece-log-decode-file-name "=23riece")))
    (lunit-assert-2
     case
     (equal "#riece:*.jp"
	    (riece-log-decode-file-name "=23riece=3A=2A=2Ejp")))
    (lunit-assert-2
     case
     (equal "#riece="
	    (riece-log-decode-file-name "=23riece==")))))

(luna-define-method test-riece-log-decode-file-name-mule
  ((case test-riece-log))
  (let ((riece-log-file-name-coding-system 'iso-8859-1))
    (lunit-assert-2
     case
     (equal (format "#%criece%c"
		     (make-char 'latin-iso8859-1 43)
		     (make-char 'latin-iso8859-1 59))
	    (riece-log-decode-file-name
	     "=23\xABriece\xBB"))))
  (let ((riece-log-file-name-coding-system 'iso-2022-jp))
    (lunit-assert-2
     case
     (equal (format "#%c%c%c"
		    (make-char 'japanese-jisx0208 36 106)
		    (make-char 'japanese-jisx0208 33 60)
		    (make-char 'japanese-jisx0208 36 57))
	    (riece-log-decode-file-name
	     "=23=1B=24B=24j=21=3C=249=1B=28B")))))

(luna-define-method test-riece-log-display-message-function
  ((case test-riece-log))
  (let ((riece-log-file-name-coding-system 'euc-jp)
	(riece-log-enabled t)
	riece-log-directory-map
	(riece-server-process-alist
	 (list (cons "" (start-process "" (current-buffer) "true")))))
    (put 'riece-log 'riece-addon-enabled t)
    (riece-log-display-message-function
     (riece-make-message
      (riece-make-identity "ueno" "")
      (riece-make-identity (format "#%c%c%c"
				   (make-char 'japanese-jisx0208 36 106)
				   (make-char 'japanese-jisx0208 33 60)
				   (make-char 'japanese-jisx0208 36 57))
			   "")
      "a b c\n" nil t))
    (put 'riece-log 'riece-addon-enabled nil))
  (let ((file-name-coding-system 'no-conversion))
    (lunit-assert-2
     case
     (file-directory-p
      (expand-file-name
       (encode-coding-string
	(format "=23%c%c%c"
		(make-char 'japanese-jisx0208 36 106)
		(make-char 'japanese-jisx0208 33 60)
		(make-char 'japanese-jisx0208 36 57))
	'euc-jp)
       riece-log-directory)))))
