; vim: ft=lisp et
(in-package :asdf)
(defsystem :jingoh.generator
  :in-order-to((test-op(test-op "jingoh.generator.test")))
  :depends-on (
               "millet" ; wrappter for implementation dependent utilities.
               "closer-mop" ; wrapper for meta object protocols.
               "lambda-list" ; utilities for lambda-list.
               "asdf" ; system loading.
               "uiop" ; utilities.
               "named-readtables" ; to manage readtable.
               "with-package" ; temporarily using package.
               "prompt-for" ; for type safe user input.
               )
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
