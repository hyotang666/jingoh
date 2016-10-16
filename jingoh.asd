; vim: ft=lisp et
(in-package :asdf)

(unless(uiop:featurep :doc-bootstrap)
  (pushnew :doc-bootstrap *features*)
  (defsystem :doc-bootstrap
    :defsystem-depends-on (:documentation-embedder)))

(defsystem :jingoh
  :version #.(demb:version :demb (first (demb:cached-file-lines "README.md")))
  :description #.(demb:description :demb (first (demb:cached-file-lines)))
  :long-description #.(format nil "~{~A~&~}"(demb:cached-file-lines))
  :license #.(demb:license :demb (find-if (demb:searcher "* License")
                                          (demb:cached-file-lines)))
  :depends-on (:jingoh.org :jingoh.tester :jingoh.reporter :jingoh.reader
                           :named-readtables :documentation-embedder)
  :in-order-to ((test-op (test-op :jingoh-test)))
  :components((:file "package")))

(defsystem :jingoh-test
  :depends-on (:jingoh :named-readtables)
  :perform (test-op(o s)
             (let((*compile-verbose* nil)
                  (*load-verbose* nil)
                  (*load-print* nil)
                  (*compile-print* nil))
               (mapc #'test-system '(:jingoh.org :jingoh.tester :jingoh.reporter :jingoh.reader)))))
