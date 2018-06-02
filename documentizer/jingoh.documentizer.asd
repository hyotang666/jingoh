; vim: ft=lisp et
(in-package :asdf)
(defsystem :jingoh.documentizer
  :depends-on (:3bmd :3bmd-ext-code-blocks :read-as-string :uiop)
  :pathname "src/"
  :components(;; bottom utils.
              (:file "utility")
              (:file "section" :depends-on ("utility"))
              ;; middle helpers depth 1.
              (:file "parse-spec" :depends-on ("section"))
              (:file "dsl" :depends-on ("section" "parse-spec"))
              ;; middle helpers depth 2.
              (:file "package" :depends-on ("dsl"))
              (:file "top" :depends-on ("package"))
              (:file "packages" :depends-on ("package"))
              (:file "about-package" :depends-on ("package"))
              (:file "about-symbols" :depends-on ("package"))
              (:file "symbol-index" :depends-on ("package"))
              (:file "table" :depends-on ("package"))
              ;; main api.
              (:file "documentize" :depends-on ("top" "packages" "about-package" "symbol-index" "about-symbols" "table"))
              ;; extension.
              (:file "github-wiki" :depends-on ("documentize"))
              ))
(defmethod component-depends-on((o test-op)(c(eql(find-system "jingoh.documentizer"))))
  (append (call-next-method)'((test-op "jingoh.documentizer.test"))))
