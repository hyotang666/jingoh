; vim: ft=lisp et
(in-package :asdf)
(defsystem :jingoh.examiner
  :version "0.0.5"
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

(defmethod operate :around ((o test-op)(c (eql (find-system "jingoh.examiner")))
                            &key ((:compile-print *compile-print*))
                            ((:compile-verbose *compile-verbose*))
                            &allow-other-keys)
  (call-next-method))
