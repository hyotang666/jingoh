; vim: ft=lisp et
(in-package :asdf)
(defsystem :jingoh.examiner
  :version "0.0.8"
  :description "Jingoh's printing issues feature."
  :long-description #.(uiop:read-file-string
                        (uiop:subpathname *load-pathname* "CONCEPTS.md"))
  :in-order-to((test-op(test-op "jingoh.examiner.test")))
  :depends-on (
               "jingoh.org" ; database.
               "jingoh.tester" ; object.
               "cl-ansi-text" ; text colorizing.
               "resignal-bind" ; condition handling.
               )
  :pathname "src/"
  :components ((:file "examine")))

;;; The form below is documentation importer.
(let((system
       (find-system "jingoh.documentizer" nil)))
  ;; Weakly depends on.
  (when system
    (load-system system)
    (defmethod operate :around((o load-op)
                               (c (eql(find-system "jingoh.examiner")))
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

(defmethod operate :around ((o test-op)(c (eql (find-system "jingoh.examiner")))
                            &key ((:compile-print *compile-print*))
                            ((:compile-verbose *compile-verbose*))
                            &allow-other-keys)
  (call-next-method))
