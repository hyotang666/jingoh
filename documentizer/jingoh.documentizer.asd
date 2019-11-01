; vim: ft=lisp et
(in-package :asdf)
(defsystem :jingoh.documentizer
  :version "1.1.7"
  :depends-on (
               "3bmd" ; markdown.
               "3bmd-ext-code-blocks"
               "read-as-string" ; s-expression string reader.
               "uiop" ; utilities.
               )
  :pathname "src/"
  :components(;; bottom utils.
              (:file "utility")
              (:file "section" :depends-on ("utility"))
              ;; middle helpers depth 1.
              (:file "parse-spec" :depends-on ("section"))
              (:file "dsl" :depends-on ("section" "parse-spec"))
              ;; middle helpers depth 2.
              (:file "package" :depends-on ("dsl"))
              (:file "table" :depends-on ("package"))
              ;; main api.
              (:file "documentize" :depends-on ("table"))
              ;; extension.
              (:file "github-wiki" :depends-on ("documentize"))
              ))
(defmethod component-depends-on((o test-op)(c(eql(find-system "jingoh.documentizer"))))
  (append (call-next-method)'((test-op "jingoh.documentizer.test"))))
(defmethod operate :around ((o test-op)(c (eql (find-system "jingoh.documentizer")))
                            &key ((:compile-print *compile-print*))
                            ((:compile-verbose *compile-verbose*))
                            &allow-other-keys)
  (call-next-method))
