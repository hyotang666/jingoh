; vim: ft=lisp et
(in-package :asdf)
(defsystem :jingoh.documentizer.test
  :depends-on
  (:jingoh "jingoh.documentizer")
  :components
  ((:file "jingoh.documentizer") (:file "jingoh.documentizer.dsl")
   (:file "jingoh.documentizer.parse-spec")
   (:file "jingoh.documentizer.utility"))
  :perform
  (test-op (o c) (symbol-call :jingoh :examine :jingoh.documentizer)
   (symbol-call :jingoh :examine :jingoh.documentizer.dsl)
   (symbol-call :jingoh :examine :jingoh.documentizer.parse-spec)
   (symbol-call :jingoh :examine :jingoh.documentizer.utility)))