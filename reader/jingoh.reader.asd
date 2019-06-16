; vim: ft=lisp et
(in-package :asdf)
(defsystem :jingoh.reader
  :version "1.0.0"
  :description "Dispatch macro for jingoh"
  :long-description #.(uiop:read-file-string
                        (uiop:subpathname *load-pathname* "CONCEPTS.md"))
  :in-order-to((test-op(test-op "jingoh.reader.test")))
  :depends-on (
               "jingoh.tester" ; reader makes this form.
               "millet" ; wrapper for implementation dependent utilities.
               "named-readtables" ; to manage readtable.
               )
  :pathname "src/"
  :components((:file "reader")))

(defmethod operate :around ((o test-op)(c (eql (find-system "jingoh.reader")))
                            &key ((:compile-print *compile-print*))
                            ((:compile-verbose *compile-verbose*))
                            &allow-other-keys)
  (call-next-method))
