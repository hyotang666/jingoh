; vim: ft=lisp et
(in-package :asdf)
(defsystem :jingoh.reader.test
  :version "0.0.1"
  :depends-on
  (:jingoh "jingoh.reader")
  :components
  ((:file "jingoh.reader"))
  :perform
  (test-op (o c) (symbol-call :jingoh :examine :jingoh.reader)))
