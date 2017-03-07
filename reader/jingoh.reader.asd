; vim: ft=lisp et
(in-package :asdf)
(defsystem :jingoh.reader
  :description "Dispatch macro for jingoh"
  :long-description #.(uiop:read-file-string
                        (uiop:subpathname "CONCEPTS.md" *load-pathname*))
  :depends-on (:jingoh.tester :millet :named-readtables :musam)
  :pathname "src/"
  :components((:file "reader")))

(defmethod perform ((o test-op) (c (eql (find-system "jingoh.reader"))))
  (test-system :jingoh.reader.test))
