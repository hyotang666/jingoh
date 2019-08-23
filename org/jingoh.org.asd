; vim: ft=lisp et
(in-package :asdf)

(defsystem :jingoh.org
  :version "0.1.3"
  :description "Jingoh's background database system"
  :long-description #.(uiop:read-file-string
                        (uiop:subpathname *load-pathname* "CONCEPTS.md"))
  :in-order-to((test-op(test-op "jingoh.org.test")))
  :pathname "src/"
  :depends-on (
               "resignal-bind" ; to condition handling.
               "alexandria" ; public domain utilities.
               "uiop" ; utilities.
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

;;; Two forms below are documentation importer.
(let((system
       (find-system "jingoh.documentizer" nil)))
  ;; Weakly depends on.
  (when system
    (load-system system)))

(defmethod operate :around((o load-op)(c (eql(find-system "jingoh.org")))&key)
  (if(not(find-package "JINGOH.DOCUMENTIZER"))
    (call-next-method)
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
              forms)))))

(defmethod operate :around ((o test-op)(c (eql (find-system "jingoh.org")))
                            &key ((:compile-print *compile-print*))
                            ((:compile-verbose *compile-verbose*))
                            &allow-other-keys)
  (call-next-method))
