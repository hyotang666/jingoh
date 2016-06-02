; vim: ft=lisp et
(in-package :asdf)
(defsystem :jingoh.reporter
  :description "Jingoh's printing issues feature."
  :depends-on (:jingoh.org :jingoh.util :jingoh.tester :with-resignal)
  :in-order-to ((test-op (test-op :jingoh.reporter-test)))
  :pathname "src/reporter/"
  :serial t
  :components ((:file "package")
	       (:file "reporters")))

(defsystem :jingoh.reporter-test
  :depends-on (:jingoh :millet :named-readtables)
  :pathname "src/reporter/"
  :components ((:file "design"))
  :perform (test-op(o s)
             (uiop:symbol-call :jingoh 'report)))

