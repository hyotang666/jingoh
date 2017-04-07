; vim: ft=lisp et
(in-package :asdf)
(defsystem :jingoh.tester
  :description "Jingoh's requirement's tester."
  :long-description #.(uiop:read-file-string (merge-pathnames *load-pathname*
                                                              "CONCEPTS.md"))
  :depends-on (:jingoh.org :millet :closer-mop :alexandria :cl-ansi-text :cl-ppcre :structure-ext :uiop)
  :pathname "src/"
  :components ((:file "package")
               ; bottom
               (:file "report" :depends-on ("package"))
	       (:file "miscellaneous" :depends-on ("package"))
               ; top
               (:file "tester" :depends-on ("miscellaneous" "report"))
	       ))

(defmethod perform ((o test-op) (c (eql (find-system "jingoh.tester"))))
  (test-system :jingoh.tester.test))
