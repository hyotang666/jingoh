; vim: ft=lisp et
(in-package :asdf)
(defsystem :jingoh.documentizer
  :depends-on (:3bmd :3bmd-ext-code-blocks :read-as-string :cl-who :uiop)
  :pathname "src/"
  :components((:file "package")
              (:file "utility" :depends-on ("package"))
              (:file "parse-spec" :depends-on ("utility"))
              (:file "dsl" :depends-on ("parse-spec"))

              (:file "top" :depends-on ("dsl"))
              (:file "packages" :depends-on ("dsl"))
              (:file "about-package" :depends-on ("dsl"))
              (:file "about-symbols" :depends-on ("dsl"))
              (:file "symbol-index" :depends-on ("dsl"))
              (:file "table" :depends-on ("dsl"))

              (:file "documentize" :depends-on ("top" "packages" "about-package" "symbol-index" "about-symbols" "table"))
              ))
;; Perform method below is added by JINGOH.GENERATOR.
(defmethod perform ((o test-op) (c (eql (find-system "jingoh.documentizer"))))
  (test-system :jingoh.documentizer.test))
