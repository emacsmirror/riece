(require 'riece-cache)

(luna-define-class test-riece-cache (lunit-test-case))

(luna-define-method test-riece-cache-get-set ((case test-riece-cache))
  (let ((cache (riece-make-cache 3)))
    (riece-cache-set cache "0" 0)
    (lunit-assert-2
     case
     (= (riece-cache-get cache "0") 0))))

(luna-define-method test-riece-cache-set-overflow ((case test-riece-cache))
  (let ((cache (riece-make-cache 3)))
    (riece-cache-set cache "0" 0)
    (lunit-assert-2
     case
     (= (riece-cache-hash-length cache)	1))
    (riece-cache-set cache "1" 1)
    (lunit-assert-2
     case
     (= (riece-cache-hash-length cache)	2))
    (riece-cache-set cache "2" 2)
    (lunit-assert-2
     case
     (= (riece-cache-hash-length cache)	3))
    (riece-cache-set cache "3" 3)
    (lunit-assert-2
     case
     (= (riece-cache-hash-length cache)	3))
    (lunit-assert-2
     case
     (null (riece-cache-get cache "0")))))

(luna-define-method test-riece-cache-get-increase-key ((case test-riece-cache))
  (let ((cache (riece-make-cache 3)))
    (riece-cache-set cache "0" 0)
    (riece-cache-set cache "1" 1)
    (riece-cache-set cache "2" 2)
    (lunit-assert-2
     case
     (equal (riece-cache-node-key (riece-cache-first cache)) "0"))
    (lunit-assert-2
     case
     (equal (riece-cache-node-key (riece-cache-last cache)) "2"))
    (riece-cache-get cache "1")
    (lunit-assert-2
     case
     (equal (riece-cache-node-key (riece-cache-last cache)) "1"))))

(luna-define-method test-riece-cache-delete ((case test-riece-cache))
  (let ((cache (riece-make-cache 3)))
    (riece-cache-set cache "0" 0)
    (riece-cache-set cache "1" 1)
    (riece-cache-set cache "2" 2)
    (riece-cache-delete cache "1")
    (lunit-assert-2
     case
     (= (riece-cache-hash-length cache) 2))
    (lunit-assert-2
     case
     (null (riece-cache-get cache "1")))
    (riece-cache-set cache "1" 1)
    (riece-cache-delete cache "0")
    (lunit-assert-2
     case
     (= (riece-cache-hash-length cache) 2))
    (lunit-assert-2
     case
     (null (riece-cache-get cache "0")))
    (riece-cache-set cache "0" 0)
    (riece-cache-delete cache "2")
    (lunit-assert-2
     case
     (= (riece-cache-hash-length cache) 2))
    (lunit-assert-2
     case
     (null (riece-cache-get cache "2")))))
