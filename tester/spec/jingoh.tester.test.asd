; vim: ft=lisp et
(in-package :asdf)
(defsystem :jingoh.tester.test
  :depends-on
  (:jingoh "jingoh.tester")
  :components
  ((:file "jingoh.tester"))
  :perform
  (test-op (o c) (symbol-call :jingoh :examine)))
