; vim: ft=lisp et
(in-package :asdf)
(defsystem :jingoh.reader
  :description "Dispatch macro for jingoh"
  :in-order-to ((test-op (test-op :jingoh.reader-test)))
  :depends-on (:jingoh.tester :millet :named-readtables :jingoh.util :musam)
  :pathname "src/reader/"
  :components((:file "reader")))

(defsystem :jingoh.reader-test
  :depends-on (:jingoh :named-readtables)
  :pathname "src/reader/"
  :components ((:file "design"))
  :perform (test-op(o s)
             (uiop:symbol-call :jingoh 'report)))

