; vim: ft=lisp et
(in-package :asdf)
(defsystem :jingoh.examiner.test
  :version "0.1.0"
  :depends-on
  (:jingoh "jingoh.examiner" :cl-ansi-text)
  :components
  ((:file "jingoh.examiner"))
  :perform
  (test-op (o c) (symbol-call :jingoh :examine :jingoh.examiner)))
