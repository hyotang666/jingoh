; vim: ft=lisp et
(in-package :asdf)
(defsystem "jingoh.parallel"
  :depends-on
  ("lparallel" "resignal-bind" "jingoh" "bordeaux-threads" "cl-cpus")
  :components
  ((:file "parallel")))

(defmethod component-depends-on((o test-op)(c (eql (find-system "jingoh.parallel"))))
  (append (call-next-method)'((test-op "jingoh.parallel.test"))))
