; vim: ft=lisp et
(in-package :asdf)
(defsystem :jingoh.org.test
  :version "0.0.0"
  :depends-on (:jingoh "jingoh.org") :components
  ((:file "jingoh.org")) :perform
  (test-op (o c) (symbol-call :jingoh :examine :jingoh.org)))
