; vim: ft=lisp et
(in-package :asdf)
(defsystem :jingoh.reader
  :description "Dispatch macro for jingoh"
  :in-order-to ((test-op (load-op :jingoh.reader-test)))
  :depends-on (:jingoh.tester :millet :named-readtables :jingoh.util :musam)
  :pathname "src/reader/"
  :components((:file "reader")))

(defsystem :jingoh.reader-test
  :depends-on (:jingoh :named-readtables)
  :pathname "src/reader/"
  :components ((:file "design"))
  :perform (load-op(o s)
             (uiop:symbol-call :jingoh 'report)))

