; vim: ft=lisp et
(in-package :asdf)
(defsystem :jingoh.org
  :description "Jingoh's background database system"
  :pathname "src/org/"
  :depends-on (:jingoh.util :with-resignal)
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
  :pathname "src/org/"
  :components ((:file "design"))
  :perform (test-op(o s)
             (uiop:symbol-call :jingoh 'report)))
