; vim: ft=lisp et
(in-package :asdf)
(defsystem :jingoh.generator
  :version "1.10.1"
  :in-order-to((test-op(test-op "jingoh.generator.test")))
  :depends-on (
               "millet" ; wrappter for implementation dependent utilities.
               "closer-mop" ; wrapper for meta object protocols.
               "lambda-fiddle" ; utilities for lambda-list.
               "asdf" ; system loading.
               "uiop" ; utilities.
               "quicklisp" ; system installing.
               "named-readtables" ; to manage readtable.
               "prompt-for" ; for type safe user input.
               "trivial-cltl2" ; wrapper for cltl2.
               "cl-unification" ; unification.
               )
  :pathname "src"
  :components ((:file "package")
               (:file "util" :depends-on ("package"))
               (:file "symbol-generate" :depends-on ("package"))

               (:file "generate" :depends-on ("symbol-generate"))
               (:file "dribble" :depends-on ("util"))
               ))

;;; The form below is documentation importer.
(let((system
       (find-system "jingoh.documentizer" nil)))
  ;; Weakly depends on.
  (when(and system
            (not(featurep :clisp)))
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

(defmethod operate :after ((o load-op)(c (eql (find-system "jingoh.generator")))&key)
  (unless(fboundp(find-symbol "FUNCTION-INFORMATION" "CLTL2"))
    (warn "TRIVIAL-CLTL2 does not support ~A so JINGOH.GENERATOR can not provide full feature."
          (lisp-implementation-type))))
