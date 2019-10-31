; vim: ft=lisp et
(in-package :asdf)
(defsystem :jingoh.generator.test
  :version "1.0.2"
  :depends-on (:jingoh "jingoh.generator")
  :components ((:file "jingoh.generator")) :perform
  (test-op (o c) (symbol-call :jingoh :examine :jingoh.generator)))
