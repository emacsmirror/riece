(require 'riece-alias)

(luna-define-class test-riece-alias (lunit-test-case))

(luna-define-method test-riece-alias-percent-hack ((case test-riece-alias))
  (let ((riece-alias-percent-hack-mask "*.jp"))
    (lunit-assert-2
     case
     (equal
      (riece-alias-abbrev-percent-hack "#riece:*.jp")
      "%riece"))
    (lunit-assert-2
     case
     (equal
      (riece-alias-expand-percent-hack "%riece")
      "#riece:*.jp"))))

(luna-define-method test-riece-alias-alist-1 ((case test-riece-alias))
  (let ((riece-alias-alist '(("#riece" . "#r"))))
    (lunit-assert-2
     case
     (equal
      (riece-alias-abbrev-identity-string "#riece")
      "#r"))
    (lunit-assert-2
     case
     (equal
      (riece-alias-expand-identity-string "#r")
      "#riece"))))

(luna-define-method test-riece-alias-alist-2 ((case test-riece-alias))
  (let ((riece-alias-alist '(("%riece" . "%r")))
	(riece-alias-percent-hack-mask "*.jp"))
    (lunit-assert-2
     case
     (equal
      (riece-alias-abbrev-identity-string "#riece:*.jp")
      "%r"))
    (lunit-assert-2
     case
     (equal
      (riece-alias-expand-identity-string "%r")
      "#riece:*.jp"))))

(luna-define-method test-riece-alias-altsep-1 ((case test-riece-alias))
  (let ((riece-alias-alternate-separator "@"))
    (lunit-assert-2
     case
     (equal
      (riece-alias-abbrev-alternate-separator "#riece")
      "#riece"))
    (lunit-assert-2
     case
     (equal
      (riece-alias-abbrev-alternate-separator "#riece localhost")
      "#riece@localhost"))
    (lunit-assert-2
     case
     (equal
      (riece-alias-abbrev-alternate-separator "#ch@nnel")
      "#ch@@nnel"))
    (lunit-assert-2
     case
     (equal
      (riece-alias-abbrev-alternate-separator "#ch@nnel localhost")
      "#ch@@nnel@localhost"))))

(luna-define-method test-riece-alias-altsep-2 ((case test-riece-alias))
  (let ((riece-alias-alternate-separator "@@"))
    (lunit-assert-2
     case
     (equal
      (riece-alias-abbrev-alternate-separator "#riece")
      "#riece"))
    (lunit-assert-2
     case
     (equal
      (riece-alias-abbrev-alternate-separator "#riece localhost")
      "#riece@@localhost"))
    (lunit-assert-2
     case
     (equal
      (riece-alias-abbrev-alternate-separator "#ch@@nnel")
      "#ch@@@@nnel"))
    (lunit-assert-2
     case
     (equal
      (riece-alias-abbrev-alternate-separator "#ch@@nnel localhost")
      "#ch@@@@nnel@@localhost"))))

(luna-define-method test-riece-alias-altsep-3 ((case test-riece-alias))
  (let ((riece-alias-alternate-separator "@"))
    (lunit-assert-2
     case
     (equal
      (riece-alias-expand-alternate-separator "#riece")
      "#riece"))
    (lunit-assert-2
     case
     (equal
      (riece-alias-expand-alternate-separator "#riece@localhost")
      "#riece localhost"))
    (lunit-assert-2
     case
     (equal
      (riece-alias-expand-alternate-separator "#ch@@nnel")
      "#ch@nnel"))
    (lunit-assert-2
     case
     (equal
      (riece-alias-expand-alternate-separator "#ch@@nnel@localhost")
      "#ch@nnel localhost"))))

(luna-define-method test-riece-alias-altsep-4 ((case test-riece-alias))
  (let ((riece-alias-alternate-separator "@@"))
    (lunit-assert-2
     case
     (equal
      (riece-alias-expand-alternate-separator "#riece")
      "#riece"))
    (lunit-assert-2
     case
     (equal
      (riece-alias-expand-alternate-separator "#riece@@localhost")
      "#riece localhost"))
    (lunit-assert-2
     case
     (equal
      (riece-alias-expand-alternate-separator "#ch@@@@nnel")
      "#ch@@nnel"))
    (lunit-assert-2
     case
     (equal
      (riece-alias-expand-alternate-separator "#ch@@@@nnel@@localhost")
      "#ch@@nnel localhost"))))