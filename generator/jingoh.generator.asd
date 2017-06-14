; vim: ft=lisp et
(in-package :asdf)
(defsystem :jingoh.generator
  :depends-on (:millet :closer-mop :lambda-list :asdf :uiop)
  :pathname "src"
  :components ((:file "package")
               (:file "asd" :depends-on ("package"))
               (:file "symbol-generate" :depends-on ("package"))

               (:file "system" :depends-on ("asd"))
               (:file "symbol" :depends-on ("system"))
               (:file "defpackage-form" :depends-on ("symbol-generate"))
               ))
(defmethod perform ((o test-op) (c (eql (find-system "jingoh.generator"))))
 (test-system :jingoh.generator.test))
