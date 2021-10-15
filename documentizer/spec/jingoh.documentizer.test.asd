; vim: ft=lisp et
(in-package :asdf)
(defsystem :jingoh.documentizer.test
  :version "1.0.5"
  :depends-on
  (:jingoh "jingoh.documentizer")
  :components
  ((:file "jingoh.documentizer"))
  :perform
  (test-op (o c) (symbol-call :jingoh :examine :jingoh.documentizer)))
