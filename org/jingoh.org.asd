; vim: ft=lisp et
(in-package :asdf)
(defsystem :jingoh.org
  :version "0.1.1"
  :description "Jingoh's background database system"
  :long-description #.(uiop:read-file-string
                        (uiop:subpathname *load-pathname* "CONCEPTS.md"))
  :in-order-to((test-op(test-op "jingoh.org.test")))
  :pathname "src/"
  :depends-on (
               "resignal-bind" ; to condition handling.
               "alexandria" ; public domain utilities.
               "uiop" ; utilities.
               )
  :components((:file "package")
              ; bottom
              (:file "conditions" :depends-on ("package"))
              (:file "org" :depends-on ("package"))
              ; mid1
	      (:file "deforg" :depends-on ("org"))
              ; top
              (:file "miscellaneous" :depends-on ("deforg" "conditions"))
              ))

(defmethod operate :around ((o test-op)(c (eql (find-system "jingoh.org")))
                            &key ((:compile-print *compile-print*))
                            ((:compile-verbose *compile-verbose*))
                            &allow-other-keys)
  (call-next-method))
