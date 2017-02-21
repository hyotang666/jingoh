; vim: ft=lisp et
(in-package :asdf)
(defsystem :jingoh.generator
  :depends-on (:millet :closer-mop :lambda-list)
  :components ((:file "generator")))
