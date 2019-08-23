; vim: ft=lisp et
(in-package :asdf)
(defsystem :jingoh.generator
  :version "0.0.13"
  :in-order-to((test-op(test-op "jingoh.generator.test")))
  :depends-on (
               "millet" ; wrappter for implementation dependent utilities.
               "closer-mop" ; wrapper for meta object protocols.
               "lambda-fiddle" ; utilities for lambda-list.
               "asdf" ; system loading.
               "uiop" ; utilities.
               "named-readtables" ; to manage readtable.
               "prompt-for" ; for type safe user input.
               )
  :pathname "src"
  :components ((:file "package")
               (:file "util" :depends-on ("package"))
               (:file "asd" :depends-on ("package"))
               (:file "init" :depends-on ("package"))
               (:file "readme" :depends-on ("package"))
               (:file "symbol-generate" :depends-on ("package"))

               (:file "system" :depends-on ("asd" "util"))
               (:file "symbol" :depends-on ("system" "util"))
               (:file "defpackage-form" :depends-on ("symbol-generate"))
               ))

;;; The form below is documentation importer.
(let((system
       (find-system "jingoh.documentizer" nil)))
  ;; Weakly depends on.
  (when system
    (load-system system)
    (defmethod operate :around((o load-op)(c (eql(find-system "jingoh.org")))&key)
      (let*((forms nil)
            (*macroexpand-hook*
              (let((outer-hook *macroexpand-hook*))
                (lambda(expander form env)
                  (when(typep form '(cons (eql defpackage)*))
                    (push form forms))
                  (funcall outer-hook expander form env))))
            (*default-pathname-defaults*
              (merge-pathnames "spec/"
                               (system-source-directory c))))
        (multiple-value-prog1(call-next-method)
          (mapc (find-symbol "IMPORTER" "JINGOH.DOCUMENTIZER")
                forms))))))

(defmethod operate :around ((o test-op)(c (eql (find-system "jingoh.generator")))
                            &key ((:compile-print *compile-print*))
                            ((:compile-verbose *compile-verbose*))
                            &allow-other-keys)
  (call-next-method))
