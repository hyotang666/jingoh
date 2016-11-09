; vim: ft=lisp et
(in-package :asdf)
(unless(uiop:featurep :doc-bootstrap)
  (pushnew :doc-bootstrap *features*)
  (defsystem :doc-bootstrap
    :defsystem-depends-on (:documentation-embedder)))

(defsystem :jingoh.tester
  :description "Jingoh's requirement's tester."
  :in-order-to ((test-op (test-op :jingoh.tester-test)))
  :depends-on (:jingoh.org :millet :closer-mop :documentation-embedder :alexandria)
  :pathname "src/"
  :components ((:file "package")
               ; bottom
               (:file "report" :depends-on ("package"))
	       (:file "miscellaneous" :depends-on ("package"))
               ; top
               (:file "tester" :depends-on ("miscellaneous" "report"))
	       ))

(defsystem :jingoh.tester-test
  :depends-on (:jingoh :named-readtables)
  :pathname "src/"
  :components ((:file "design"))
  :perform (test-op(o s)
             (uiop:symbol-call :jingoh 'report)))

