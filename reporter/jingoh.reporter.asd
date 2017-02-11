; vim: ft=lisp et
(in-package :asdf)
(unless(uiop:featurep :doc-bootstrap)
  (pushnew :doc-bootstrap *features*)
  (defsystem :doc-bootstrap
    :defsystem-depends-on (:documentation-embedder)))

(defsystem :jingoh.reporter
  :description "Jingoh's printing issues feature."
  :depends-on (:jingoh.org :jingoh.tester :resignal-bind :documentation-embedder)
  :in-order-to ((test-op (test-op :jingoh.reporter-test)))
  :pathname "src/"
  :serial t
  :components ((:file "package")
	       (:file "reporters")))

(defsystem :jingoh.reporter-test
  :depends-on (:jingoh :millet :named-readtables)
  :pathname "src/"
  :components ((:file "design"))
  :perform (test-op(o s)
             (uiop:symbol-call :jingoh 'verify :jingoh.reporter)))

