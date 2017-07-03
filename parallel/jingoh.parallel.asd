; vim: ft=lisp et
(in-package :asdf)
(defsystem "jingoh.parallel"
  :depends-on
  ("lparallel" "resignal-bind" "jingoh" "bordeaux-threads" "cl-cpus")
  :components
  ((:file "parallel")))
;; Perform method below is added by JINGOH.GENERATOR.
(defmethod perform ((o test-op) (c (eql (find-system "jingoh.parallel"))))
  (test-system :jingoh.parallel.test))
