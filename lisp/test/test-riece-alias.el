(require 'riece-alias)

(luna-define-class test-riece-alias (lunit-test-case))

(luna-define-method test-riece-alias-altsep-1 ((case test-riece-alias))
  (let ((riece-alias-alternate-separator "@"))
    (lunit-assert
     (equal
      (riece-alias-abbrev-alternate-separator "#riece")
      "#riece"))
    (lunit-assert
     (equal
      (riece-alias-abbrev-alternate-separator "#riece localhost")
      "#riece@localhost"))
    (lunit-assert
     (equal
      (riece-alias-abbrev-alternate-separator "#ch@nnel")
      "#ch@@nnel"))
    (lunit-assert
     (equal
      (riece-alias-abbrev-alternate-separator "#ch@nnel localhost")
      "#ch@@nnel@localhost"))))

(luna-define-method test-riece-alias-altsep-2 ((case test-riece-alias))
  (let ((riece-alias-alternate-separator "@@"))
    (lunit-assert
     (equal
      (riece-alias-abbrev-alternate-separator "#riece")
      "#riece"))
    (lunit-assert
     (equal
      (riece-alias-abbrev-alternate-separator "#riece localhost")
      "#riece@@localhost"))
    (lunit-assert
     (equal
      (riece-alias-abbrev-alternate-separator "#ch@@nnel")
      "#ch@@@@nnel"))
    (lunit-assert
     (equal
      (riece-alias-abbrev-alternate-separator "#ch@@nnel localhost")
      "#ch@@@@nnel@@localhost"))))

(luna-define-method test-riece-alias-altsep-3 ((case test-riece-alias))
  (let ((riece-alias-alternate-separator "@"))
    (lunit-assert
     (equal
      (riece-alias-expand-alternate-separator "#riece")
      "#riece"))
    (lunit-assert
     (equal
      (riece-alias-expand-alternate-separator "#riece@localhost")
      "#riece localhost"))
    (lunit-assert
     (equal
      (riece-alias-expand-alternate-separator "#ch@@nnel")
      "#ch@nnel"))
    (lunit-assert
     (equal
      (riece-alias-expand-alternate-separator "#ch@@nnel@localhost")
      "#ch@nnel localhost"))))

(luna-define-method test-riece-alias-altsep-4 ((case test-riece-alias))
  (let ((riece-alias-alternate-separator "@@"))
    (lunit-assert
     (equal
      (riece-alias-expand-alternate-separator "#riece")
      "#riece"))
    (lunit-assert
     (equal
      (riece-alias-expand-alternate-separator "#riece@@localhost")
      "#riece localhost"))
    (lunit-assert
     (equal
      (riece-alias-expand-alternate-separator "#ch@@@@nnel")
      "#ch@@nnel"))
    (lunit-assert
     (equal
      (riece-alias-expand-alternate-separator "#ch@@@@nnel@@localhost")
      "#ch@@nnel localhost"))))