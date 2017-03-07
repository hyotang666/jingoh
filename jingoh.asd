; vim: ft=lisp et
(in-package :asdf)
(defsystem :jingoh
  :description "DSL to notate specification, rather than test framework."
  :long-description #.(uiop:read-file-string
                        (uiop:subpathname "CONCEPTS.md" *load-pathname*))
  :depends-on (:jingoh.org :jingoh.tester :jingoh.examiner :jingoh.reader
                           :named-readtables)
  :in-order-to ((test-op (test-op :jingoh-test)))
  :components((:file "package")))

(defsystem :jingoh-test
  :depends-on (:jingoh :named-readtables)
  :perform (test-op(o s)
             (let((*compile-verbose* nil)
                  (*load-verbose* nil)
                  (*load-print* nil)
                  (*compile-print* nil))
               (mapc #'test-system '(:jingoh.org :jingoh.tester :jingoh.examiner :jingoh.reader)))))
