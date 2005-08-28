(require 'riece-addon)

(defun test-riece-addon-1-requires ()
  '(test-riece-addon-2))
(provide 'test-riece-addon-1)

(defun test-riece-addon-2-requires ()
  '(test-riece-addon-3 test-riece-addon-4))
(provide 'test-riece-addon-2)

(defun test-riece-addon-3-requires ())
(provide 'test-riece-addon-3)

(defun test-riece-addon-4-requires ())
(provide 'test-riece-addon-4)

(defun test-riece-addon-5-requires ()
  '(test-riece-addon-6))
(provide 'test-riece-addon-5)

(defun test-riece-addon-6-requires ()
  '(test-riece-addon-5))
(provide 'test-riece-addon-6)

(luna-define-class test-riece-addon (lunit-test-case))

(luna-define-method test-riece-resolve-addons-1 ((case test-riece-addon))
  (lunit-assert-2
   case
   (equal
    (mapcar #'car (riece-resolve-addons
		   '(test-riece-addon-1 test-riece-addon-2)))
    '(test-riece-addon-3 test-riece-addon-4
			 test-riece-addon-2 test-riece-addon-1))))

(luna-define-method test-riece-resolve-addons-2 ((case test-riece-addon))
  (lunit-assert-2
   case
   (condition-case error
       (progn
	 (riece-resolve-addons '(test-riece-addon-5 test-riece-addon-6))
	 nil)
     (error (nth 1 error)))))
