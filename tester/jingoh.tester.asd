; vim: ft=lisp et
(in-package :asdf)
(defsystem :jingoh.tester
  :description "Jingoh's requirement's tester."
  :version "0.1.0"
  :long-description #.(uiop:read-file-string (merge-pathnames *load-pathname*
                                                              "CONCEPTS.md"))
  :in-order-to((test-op(test-op "jingoh.tester.test")))
  :depends-on (
               "jingoh.org" ; database.
               "millet" ; Wrapper for implementation dependent utilities.
               "closer-mop" ; wrapper for meta object protocol.
               "alexandria" ; public domain utilities.
               "cl-ansi-text" ; text colorizing.
               "cl-ppcre" ; regular expression.
               "structure-ext" ; to enable constructing structure with MAKE-INSTANCE.
               "uiop" ; utilities.
               "bordeaux-threads"       ; multi threading especially for timeout.
               "resignal-bind" ; condition capturing for better error message.
               )
  :pathname "src/"
  :components ((:file "package")
               ; bottom
               (:file "report" :depends-on ("package"))
	       (:file "miscellaneous" :depends-on ("package"))
               ; top
               (:file "tester" :depends-on ("miscellaneous" "report"))
	       ))

;;; The form below is documentation importer.
(let((system
       (find-system "jingoh.documentizer" nil)))
  ;; Weakly depends on.
  (when system
    (load-system system)
    (defmethod operate :around((o load-op)
                               (c (eql(find-system "jingoh.tester")))
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

(defmethod operate :around ((o test-op)(c (eql (find-system "jingoh.tester")))
                            &key ((:compile-print *compile-print*))
                            ((:compile-verbose *compile-verbose*))
                            &allow-other-keys)
  (call-next-method))
