; vim: ft=lisp et
(in-package :asdf)
(defsystem :jingoh.reader.test
  :depends-on
  (:jingoh "jingoh.reader")
  :components
  ((:file "jingoh.reader"))
  :perform
  (test-op (o c) (symbol-call :jingoh :examine :jingoh.reader)))
