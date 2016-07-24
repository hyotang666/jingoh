; vim: ft=lisp et
(in-package :asdf)
(defsystem :jingoh
  :description "A test framework for supporting requirements first development."
  :depends-on (:jingoh.org :jingoh.tester :jingoh.reporter :jingoh.reader :jingoh.util :named-readtables)
  :in-order-to ((test-op (test-op :jingoh-test)))
  :pathname "src/jingoh/"
  :components((:file "package")))

(defsystem :jingoh-test
  :depends-on (:jingoh :named-readtables)
  :perform (test-op(o s)
             (let((*compile-verbose* nil)
                  (*load-verbose* nil)
                  (*load-print* nil)
                  (*compile-print* nil))
               (mapc #'test-system '(:jingoh.org :jingoh.tester :jingoh.reporter :jingoh.reader)))))
