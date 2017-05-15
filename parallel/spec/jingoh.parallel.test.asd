; vim: ft=lisp et
(in-package :asdf)
(defsystem :jingoh.parallel.test
  :depends-on
  (:jingoh "jingoh.parallel")
  :components
  ((:file "jingoh.parallel"))
  :perform
  (test-op (o c) (symbol-call :jingoh :examine :jingoh.parallel)))