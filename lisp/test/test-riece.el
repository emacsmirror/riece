(require 'riece)

(luna-define-class test-riece (lunit-test-case))

(luna-define-method test-riece-shrink-buffer ((case test-riece))
  (let* ((riece-max-buffer-size 255)
	 (riece-shrink-buffer-remove-chars 128)
	 (line (concat (make-string 15 ? ) "\n"))
	 (index 0))
    (with-temp-buffer
      (while (< index 16)
	(insert line)
	(setq index (1+ index)))
      (riece-shrink-buffer (current-buffer))
      (lunit-assert-2
       case
       (equal
	(apply #'concat (make-list 8 line))
	(buffer-string))))))
