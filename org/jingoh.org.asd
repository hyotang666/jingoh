; vim: ft=lisp et
(in-package :asdf)
(unless(uiop:featurep :doc-bootstrap)
  (pushnew :doc-bootstrap *features*)
  (defsystem :doc-bootstrap
    :defsystem-depends-on (:documentation-embedder)))

(defsystem :jingoh.org
  :description "Jingoh's background database system"
  :pathname "src/"
  :depends-on (:resignal-bind :documentation-embedder :alexandria)
  :in-order-to ((test-op (test-op :jingoh.org-test)))
  :components((:file "package")
              ; bottom
              (:file "conditions" :depends-on ("package"))
              (:file "org" :depends-on ("package"))
              ; mid1
	      (:file "deforg" :depends-on ("org"))
              ; top
              (:file "miscellaneous" :depends-on ("deforg" "conditions"))
              ))

(defsystem :jingoh.org-test
  :depends-on (:jingoh :named-readtables)
  :pathname "src/"
  :components ((:file "design"))
  :perform (test-op(o s)
             (uiop:symbol-call :jingoh 'verify :jingoh.org)))
