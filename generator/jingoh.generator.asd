; vim: ft=lisp et
(in-package :asdf)
(defsystem :jingoh.generator
  :depends-on (:millet :closer-mop :lambda-list)
  :pathname "src"
  :components ((:file "package")
               (:file "asd" :depends-on ("package"))
               (:file "symbol-generate" :depends-on ("package"))
               (:file "system" :depends-on ("asd"))
               (:file "symbol" :depends-on ("asd"))
               (:file "defpackage-form" :depends-on ("symbol-generate"))
               ))
