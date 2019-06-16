; vim: ft=lisp et
(in-package :asdf)
(defsystem :jingoh
  :version "1.0.1"
  :description "DSL to notate specification, rather than test framework."
  :author "Shinichi Sato"
  :license "MIT"
  :long-description #.(uiop:read-file-string
                        (uiop:subpathname *load-pathname* "CONCEPTS.md"))
  :depends-on (:jingoh.org :jingoh.tester :jingoh.examiner :jingoh.reader
                           :named-readtables)
  :in-order-to ((test-op (test-op :jingoh/test)))
  :components((:file "package")))

(defsystem :jingoh/test
  :depends-on (:jingoh :named-readtables)
  :in-order-to ((test-op(test-op "jingoh.org" "jingoh.tester" "jingoh.examiner" "jingoh.reader"))))

(defmethod operate :around((o test-op)(c (eql (find-system "jingoh")))
                           &key ((:compile-print *compile-print*))
                           ((:compile-verbose *compile-verbose*))
                           &allow-other-keys)
  (call-next-method))
