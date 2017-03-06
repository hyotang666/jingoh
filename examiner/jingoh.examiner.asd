; vim: ft=lisp et
(in-package :asdf)
(defsystem :jingoh.examiner
  :description "Jingoh's printing issues feature."
  :depends-on (:jingoh.org :jingoh.tester :resignal-bind :cl-ansi-text)
  :pathname "src/"
  :serial t
  :components ((:file "package")
	       (:file "examine")))

(defmethod perform ((o test-op) (c (eql (find-system "jingoh.examiner"))))
  (test-system :jingoh.examiner.test))
