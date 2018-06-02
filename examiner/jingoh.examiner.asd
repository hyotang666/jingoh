; vim: ft=lisp et
(in-package :asdf)
(defsystem :jingoh.examiner
  :description "Jingoh's printing issues feature."
  :long-description #.(uiop:read-file-string
                        (uiop:subpathname *load-pathname* "CONCEPTS.md"))
  :in-order-to((test-op(test-op "jingoh.examiner.test")))
  :depends-on (
               "jingoh.org" ; database.
               "jingoh.tester" ; object.
               "cl-ansi-text" ; text colorizing.
               "resignal-bind" ; condition handling.
               )
  :pathname "src/"
  :components ((:file "examine")))
