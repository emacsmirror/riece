(require 'riece-url)

(luna-define-class test-riece-url (lunit-test-case))

(luna-define-method test-riece-url-replace-match ((case test-riece-url))
  (with-temp-buffer
    (insert "111\n222\n333\n")
    (goto-char (point-min))
    (re-search-forward "2\\(2\\)2")
    (lunit-assert-2
     case
     (equal (save-match-data
	      (riece-url-replace-match "\\&"))
	    "222"))
    (lunit-assert-2
     case
     (equal (save-match-data
	      (riece-url-replace-match "\\0"))
	    "\\0"))
    (lunit-assert-2
     case
     (equal (save-match-data
	      (riece-url-replace-match "\\1"))
	    "2"))
    (lunit-assert-2
     case
     (equal (save-match-data
	      (riece-url-replace-match "\\\\"))
	    "\\"))))

(luna-define-method test-riece-url-regexp-alist ((case test-riece-url))
  (with-temp-buffer
    (insert "Bug#12345\n")
    (let ((riece-url-regexp-alist
	   '(("\\bBug#\\([0-9]+\\)\\b" .
	      "http://bugs.debian.org/\\1")))
	  riece-urls
	  riece-addons)
      (riece-url-scan-region (point-min) (point-max))
      (lunit-assert-2
       case
       (member "http://bugs.debian.org/12345" riece-urls)))))
