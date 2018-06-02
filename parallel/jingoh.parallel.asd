; vim: ft=lisp et
(in-package :asdf)
(defsystem "jingoh.parallel"
  :depends-on
  (
   "lparallel"
   "resignal-bind" ; condition handling.
   "jingoh"
   "bordeaux-threads" ; wrapper for multi threading.
   "cl-cpus" ; getting CPU information.
   )
  :components
  ((:file "parallel")))

(defmethod component-depends-on((o test-op)(c (eql (find-system "jingoh.parallel"))))
  (append (call-next-method)'((test-op "jingoh.parallel.test"))))
