; vim: ft=lisp et
(in-package :asdf)
(defsystem :jingoh.documentizer.test
  :depends-on
  (:jingoh "jingoh.documentizer")
  :components
  ((:file "jingoh.documentizer"))
  :perform
  (test-op (o c) (symbol-call :jingoh :examine :jingoh.documentizer)))
