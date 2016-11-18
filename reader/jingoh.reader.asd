; vim: ft=lisp et
(in-package :asdf)
(unless(uiop:featurep :doc-bootstrap)
  (pushnew :doc-bootstrap *features*)
  (defsystem :doc-bootstrap
    :defsystem-depends-on (:documentation-embedder)))

(defsystem :jingoh.reader
  :description "Dispatch macro for jingoh"
  :in-order-to ((test-op (test-op :jingoh.reader-test)))
  :depends-on (:jingoh.tester :millet :named-readtables :musam :documentation-embedder)
  :pathname "src/"
  :components((:file "reader")))

(defsystem :jingoh.reader-test
  :depends-on (:jingoh :named-readtables)
  :pathname "src/"
  :components ((:file "design"))
  :perform (test-op(o s)
             (uiop:symbol-call :jingoh 'report :jingoh.reader)))

