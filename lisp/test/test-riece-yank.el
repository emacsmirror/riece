(require 'riece-yank)

(luna-define-class test-riece-yank (lunit-test-case))

(luna-define-method test-riece-yank-strip-space ((case test-riece-yank))
  (lunit-assert-2
   case
   (equal
    (riece-yank-strip-space "\
  def a
    0.times do
      0.times do
	p 1 # this line begins with a TAB
      end
    end
  end

")
    "\
def a
  0.times do
    0.times do
      p 1 # this line begins with a TAB
    end
  end
end")))
