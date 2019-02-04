; vim: ft=lisp et
(in-package :asdf)
(defsystem :jingoh
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
  :in-order-to ((test-op(test-op "jingoh.org" "jingoh.tester" "jingoh.examiner" "jingoh.reader")))
  :perform (test-op :around (o s)
             (let((*compile-verbose* nil)
                  (*load-verbose* nil)
                  (*load-print* nil)
                  (*compile-print* nil))
               (call-next-method))))
