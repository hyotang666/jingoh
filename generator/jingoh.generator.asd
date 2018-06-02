; vim: ft=lisp et
(in-package :asdf)
(defsystem :jingoh.generator
  :in-order-to((test-op(test-op "jingoh.generator.test")))
  :depends-on (:millet :closer-mop :lambda-list :asdf :uiop named-readtables :with-package :prompt-for)
  :pathname "src"
  :components ((:file "package")
               (:file "asd" :depends-on ("package"))
               (:file "init" :depends-on ("package"))
               (:file "readme" :depends-on ("package"))
               (:file "symbol-generate" :depends-on ("package"))

               (:file "system" :depends-on ("asd"))
               (:file "symbol" :depends-on ("system"))
               (:file "defpackage-form" :depends-on ("symbol-generate"))
               ))
