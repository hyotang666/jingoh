; vim: ft=lisp et
(in-package :asdf)
(defsystem :jingoh.tester
  :description "Jingoh's requirement's tester."
  :in-order-to ((test-op (load-op :jingoh.tester-test)))
  :depends-on (:jingoh.org :millet :jingoh.util :closer-mop)
  :pathname "src/tester/"
  :components ((:file "package")
               ; bottom
               (:file "report" :depends-on ("package"))
	       (:file "miscellaneous" :depends-on ("package"))
               ; top
               (:file "tester" :depends-on ("miscellaneous" "report"))
	       ))

(defsystem :jingoh.tester-test
  :depends-on (:jingoh :named-readtables)
  :pathname "src/tester/"
  :components ((:file "design"))
  :perform (load-op(o s)
             (uiop:symbol-call :jingoh 'report)))

