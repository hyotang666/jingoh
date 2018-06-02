; vim: ft=lisp et
(in-package :asdf)
(defsystem :jingoh.reader
  :description "Dispatch macro for jingoh"
  :long-description #.(uiop:read-file-string
                        (uiop:subpathname *load-pathname* "CONCEPTS.md"))
  :in-order-to((test-op(test-op "jingoh.reader.test")))
  :depends-on (:jingoh.tester :millet :named-readtables :musam)
  :pathname "src/"
  :components((:file "reader")))
