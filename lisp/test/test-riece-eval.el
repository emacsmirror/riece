(require 'riece-eval)

(luna-define-class test-riece-eval (lunit-test-case))

(luna-define-method test-riece-eval-string ((case test-riece-eval))
  (lunit-assert-2
   case
   (equal
    (riece-eval-form "\"abc\"")
    "abc"))
  (lunit-assert-2
   case
   (equal
    (riece-eval-form "(concat \"d\" \"e\" \"f\")")
    "def")))

(luna-define-method test-riece-eval-list ((case test-riece-eval))
  (lunit-assert-2
   case
   (equal
    (riece-eval-form "'(a b c)")
    "(a b c)"))
  (lunit-assert-2
   case
   (equal
    (riece-eval-form "(cons 'd \"e\")")
    "(d . \"e\")")))

(luna-define-method test-riece-eval-number ((case test-riece-eval))
  (lunit-assert-2
   case
   (equal
    (riece-eval-form "123")
    "123"))
  (lunit-assert-2
   case
   (equal
    (riece-eval-form "(+ 123 45.6)")
    "168.6")))

(luna-define-method test-riece-eval-nil ((case test-riece-eval))
  (lunit-assert-2
   case
   (equal
    (riece-eval-form "nil")
    ""))
  (lunit-assert-2
   case
   (equal
    (riece-eval-form "'()")
    "")))

(luna-define-method test-riece-eval-error ((case test-riece-eval))
  (let (riece-eval-ignore-error symbol)
    (makunbound 'symbol)
    (lunit-assert-2
     case
     (equal
      (riece-eval-form "symbol")
      "Error evaluating symbol: (void-variable symbol)"))
    (setq riece-eval-ignore-error t)
    (lunit-assert-2
     case
     (equal
      (riece-eval-form "symbol")
      nil))))
