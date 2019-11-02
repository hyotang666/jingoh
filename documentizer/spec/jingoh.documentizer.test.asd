; vim: ft=lisp et
(in-package :asdf)
(defsystem :jingoh.documentizer.test
  :version "0.0.1"
  :depends-on
  (:jingoh "jingoh.documentizer")
  :components
  ((:file "jingoh.documentizer"))
  :perform
  (test-op (o c) (symbol-call :jingoh :examine :jingoh.documentizer)))
