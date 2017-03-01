; vim: ft=lisp et
(in-package :asdf)
(unless(uiop:featurep :doc-bootstrap)
  (pushnew :doc-bootstrap *features*)
  (defsystem :doc-bootstrap
    :defsystem-depends-on (:documentation-embedder)))

(defsystem :jingoh.reader
  :description "Dispatch macro for jingoh"
  :depends-on (:jingoh.tester :millet :named-readtables :musam :documentation-embedder)
  :pathname "src/"
  :components((:file "reader")))

(defmethod perform ((o test-op) (c (eql (find-system "jingoh.reader"))))
  (test-system :jingoh.reader.test))
