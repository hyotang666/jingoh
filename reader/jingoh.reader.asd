; vim: ft=lisp et
(in-package :asdf)
(defsystem :jingoh.reader
  :version "2.1.9"
  :description "Dispatch macro for jingoh"
  :long-description #.(uiop:read-file-string
                        (uiop:subpathname *load-pathname* "CONCEPTS.md"))
  :in-order-to((test-op(test-op "jingoh.reader.test")))
  :depends-on (
               "jingoh.tester" ; reader makes this form.
               "named-readtables" ; to manage readtable.
               )
  :pathname "src/"
  :components((:file "reader")))

;;; The form below is documentation importer.
(let((system
       (find-system "jingoh.documentizer" nil)))
  ;; Weakly depends on.
  (when(and system
            (not(featurep :clisp)))
    (load-system system)
    (defmethod operate :around((o load-op)
                               (c (eql(find-system "jingoh.reader")))
                               &key)
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

(defmethod operate :around ((o test-op)(c (eql (find-system "jingoh.reader")))
                            &key ((:compile-print *compile-print*))
                            ((:compile-verbose *compile-verbose*))
                            &allow-other-keys)
  (call-next-method))
