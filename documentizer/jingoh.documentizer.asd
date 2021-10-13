; vim: ft=lisp et
(in-package :asdf)
(defsystem :jingoh.documentizer
  :version "6.7.1"
  :author "SATO Shinichi"
  :license "MIT"
  :description "Jingoh extension: Import/Convert specification documentation."
  :depends-on (
               "uiop"                   ; Utilities, implicitly depends on via asdf.
               "asdf"                   ; System builder.
               "3bmd"                   ; Markdown.
               "3bmd-ext-code-blocks"   ; Extension for 3bmd.
               "read-as-string"         ; S-expression string reader.
               "null-package"           ; Safe S expression reader.
               "cl-ppcre"               ; Regular expression.
               )
  :pathname "src/"
  :components(;; package.
              (:file "package")
              ;; bottom utils.
              (:file "utility" :depends-on ("package"))

              ;; middle helpers depth 1.
              (:file "section" :depends-on ("utility"))
              ;; middle helpers depth 2.
              (:file "parse-spec" :depends-on ("section" "utility"))
              ;; middle helpers depth 3.
              (:file "meta-data" :depends-on ("package" "parse-spec" "section"))

              ;; main api.
              (:file "documentize" :depends-on ("package" "meta-data"))
              (:file "importer" :depends-on ("meta-data" "section" "utility"))
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
