; vim: ft=lisp et
(in-package :asdf)
(defsystem :jingoh.org
  :description "Jingoh's background database system"
  :long-description #.(uiop:read-file-string
                        (uiop:subpathname *load-pathname* "CONCEPTS.md"))
  :pathname "src/"
  :depends-on (:resignal-bind :alexandria :uiop)
  :components((:file "package")
              ; bottom
              (:file "conditions" :depends-on ("package"))
              (:file "org" :depends-on ("package"))
              ; mid1
	      (:file "deforg" :depends-on ("org"))
              ; top
              (:file "miscellaneous" :depends-on ("deforg" "conditions"))
              ))

(defmethod perform ((o test-op)(c (eql (find-system :jingoh.org))))
  (test-system :jingoh.org.test))
