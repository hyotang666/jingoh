; vim: ft=lisp et
(in-package :asdf)
(defsystem :jingoh.tester.test
  :version "0.0.16"
  :depends-on
  (:jingoh "jingoh.tester")
  :components
  ((:file "jingoh.tester"))
  :perform
  (test-op (o c) (symbol-call :jingoh :examine :jingoh.tester)))
