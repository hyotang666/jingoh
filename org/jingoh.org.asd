; vim: ft=lisp et
(in-package :asdf)

(defsystem :jingoh.org
  :version "0.1.6"
  :description "Jingoh's background database system"
  :long-description #.(uiop:read-file-string
                        (uiop:subpathname *load-pathname* "CONCEPTS.md"))
  :in-order-to((test-op(test-op "jingoh.org.test")))
  :pathname "src/"
  :depends-on (
               "resignal-bind" ; to condition handling.
               "alexandria" ; public domain utilities.
               "uiop" ; utilities.
               "check-bnf" ; Macro arguments checker.
               )
  :components((:file "package")
              ; bottom
              (:file "conditions" :depends-on ("package"))
              (:file "org" :depends-on ("package"))
              ; mid1
	      (:file "deforg" :depends-on ("org"))
              ; top
              (:file "miscellaneous" :depends-on ("deforg" "conditions"))
              ))

;;; The form below is documentation importer.
(let((system
       (find-system "jingoh.documentizer" nil)))
  ;; Weakly depends on.
  (when system
    (load-system system)
    (defmethod operate :around((o load-op)(c (eql(find-system "jingoh.org")))&key)
      (let*((seen nil)
            (*default-pathname-defaults*
              (merge-pathnames "spec/"
                               (system-source-directory c)))
            (*macroexpand-hook*
              (let((outer-hook *macroexpand-hook*))
                (lambda(expander form env)
                  (if(not(typep form '(cons (eql defpackage)*)))
                    (funcall outer-hook expander form env)
                    (if(find (cadr form) seen :test #'string=)
                      (funcall outer-hook expander form env)
                      (progn (push (cadr form) seen)
                             `(progn ,form ,@(uiop:symbol-call "JINGOH.DOCUMENTIZER"
                                                               "IMPORTER"
                                                               form)))))))))
        (call-next-method)))))

(defmethod operate :around ((o test-op)(c (eql (find-system "jingoh.org")))
                            &key ((:compile-print *compile-print*))
                            ((:compile-verbose *compile-verbose*))
                            &allow-other-keys)
  (call-next-method))
