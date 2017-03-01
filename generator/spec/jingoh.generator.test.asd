; vim: ft=lisp et
(in-package :asdf)
(defsystem :jingoh.generator.test :depends-on (:jingoh "jingoh.generator")
 :components ((:file "nil")) :perform
 (test-op (o c) (symbol-call :jingoh :examine)))
