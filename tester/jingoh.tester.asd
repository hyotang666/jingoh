; vim: ft=lisp et
(in-package :asdf)
(unless(uiop:featurep :doc-bootstrap)
  (pushnew :doc-bootstrap *features*)
  (defsystem :doc-bootstrap
    :defsystem-depends-on (:documentation-embedder)))

(defsystem :jingoh.tester
  :description "Jingoh's requirement's tester."
  :depends-on (:jingoh.org :millet :closer-mop :documentation-embedder :alexandria :cl-ansi-text :cl-ppcre :structure-ext)
  :pathname "src/"
  :components ((:file "package")
               ; bottom
               (:file "report" :depends-on ("package"))
	       (:file "miscellaneous" :depends-on ("package"))
               ; top
               (:file "tester" :depends-on ("miscellaneous" "report"))
	       ))

(defmethod perform ((o test-op) (c (eql (find-system "jingoh.tester"))))
  (test-system :jingoh.tester.test))
