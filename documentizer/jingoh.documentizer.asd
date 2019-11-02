; vim: ft=lisp et
(in-package :asdf)
(defsystem :jingoh.documentizer
  :version "1.1.9"
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
              ;; middle helpers depth 2.
              (:file "package" :depends-on ("utility" "section" "parse-spec"))

              (:file "meta-data" :depends-on ("package" "parse-spec" "section"))
              ;; main api.
              (:file "documentize" :depends-on ("package" "meta-data"))
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
