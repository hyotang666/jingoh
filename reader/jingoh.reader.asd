; vim: ft=lisp et
(in-package :asdf)
(defsystem :jingoh.reader
  :description "Dispatch macro for jingoh"
  :long-description #.(uiop:read-file-string
                        (uiop:subpathname *load-pathname* "CONCEPTS.md"))
  :in-order-to((test-op(test-op "jingoh.reader.test")))
  :depends-on (
               "jingoh.tester" ; reader makes this form.
               "millet" ; wrapper for implementation dependent utilities.
               "named-readtables" ; to manage readtable.
               "musam" ; to recycling #` dispatch macro.
               )
  :pathname "src/"
  :components((:file "reader")))
