(require 'riece-ruby)

(luna-define-class test-riece-ruby (lunit-test-case))

(luna-define-method lunit-test-case-teardown ((case test-riece-ruby))
  (setq riece-ruby-output-handler-alist nil
	riece-ruby-output-queue-alist nil)
  (riece-ruby-execute "exit!")
  (sleep-for 1))

(luna-define-method test-riece-ruby-execute-1 ((case test-riece-ruby))
  (lunit-assert-2
   case
   (equal (riece-ruby-execute "sleep 30") "0"))
  (lunit-assert-2
   case
   (equal (riece-ruby-execute "1 << 32") "1"))
  (lunit-assert-2
   case
   (equal (riece-ruby-execute "\"") "2")))

(luna-define-method test-riece-ruby-inspect-1 ((case test-riece-ruby))
  (let ((name (riece-ruby-execute "sleep 30")))
    (lunit-assert-2
     case
     (eq (car (car (riece-ruby-inspect name))) 'OK))
    (lunit-assert-2
     case
     (assoc "running" (nth 2 (riece-ruby-inspect name))))))

(luna-define-method test-riece-ruby-inspect-2 ((case test-riece-ruby))
  (let ((name (riece-ruby-execute "1 << 32")))
    (lunit-assert-2
     case
     (eq (car (car (riece-ruby-inspect name))) 'OK))
    (lunit-assert-2
     case
     (equal (nth 1 (riece-ruby-inspect name)) "4294967296"))
    (lunit-assert-2
     case
     (assoc "finished" (nth 2 (riece-ruby-inspect name))))))

(luna-define-method test-riece-ruby-inspect-3 ((case test-riece-ruby))
  (let ((name (riece-ruby-execute "\"")))
    (lunit-assert-2
     case
     (eq (car (car (riece-ruby-inspect name))) 'OK))
    (lunit-assert-2
     case
     (equal (nth 1 (riece-ruby-inspect name))
	    "unterminated string meets end of file"))
    (lunit-assert-2
     case
     (assoc "exited" (nth 2 (riece-ruby-inspect name))))))

(luna-define-method test-riece-ruby-clear-1 ((case test-riece-ruby))
  (let ((name (riece-ruby-execute "sleep 30")))
    (riece-ruby-clear name)
    (lunit-assert-2
     case
     (eq (car (car (riece-ruby-inspect name))) 'ERR))
    (lunit-assert-2
     case
     (= (nth 1 (car (riece-ruby-inspect name))) 105))))

(defvar test-riece-ruby-exit-handler-1 nil)
(luna-define-method test-riece-ruby-exit-handler-1 ((case test-riece-ruby))
  (let ((name (riece-ruby-execute "1 << 32")))
    (riece-ruby-set-exit-handler
     name
     (lambda (name)
       (setq test-riece-ruby-exit-handler-1 name)))
    (sleep-for 1)
    (lunit-assert-2
     case
     (equal test-riece-ruby-exit-handler-1 name))))

(defvar test-riece-ruby-output-handler-1 nil)
(luna-define-method test-riece-ruby-output-handler-1 ((case test-riece-ruby))
  (let ((name (riece-ruby-execute "output(1 << 32)")))
    (riece-ruby-set-output-handler
     name
     (lambda (name output time)
       (setq test-riece-ruby-output-handler-1 output)))
    (sleep-for 1)
    (lunit-assert-2
     case
     (equal test-riece-ruby-output-handler-1 "4294967296"))))

(defvar test-riece-ruby-output-handler-2 nil)
(luna-define-method test-riece-ruby-output-handler-2 ((case test-riece-ruby))
  (let ((name (riece-ruby-execute "output(1 << 32)")))
    (sleep-for 1)
    (riece-ruby-set-output-handler
     name
     (lambda (name output time)
       (setq test-riece-ruby-output-handler-2 output)))
    (lunit-assert-2
     case
     (equal test-riece-ruby-output-handler-2 "4294967296"))))
