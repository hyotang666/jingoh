; vim: ft=lisp et
(in-package :asdf)
(defsystem :jingoh.tester
  :description "Jingoh module for issue objects and interface for jingoh.org."
  :version "1.1.14"
  :author "SATO Shinichi"
  :license "MIT"
  :long-description #.(uiop:read-file-string (merge-pathnames "CONCEPTS.md"
                                                              *load-pathname*))
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
               "check-bnf" ; Macro arguments checker.
               )
  :pathname "src/"
  :components ((:file "package")
               ; bottom
               (:file "report" :depends-on ("package"))
	       (:file "miscellaneous" :depends-on ("package"))
               ; top
               (:file "tester" :depends-on ("miscellaneous" "report"))
	       ))

(defmethod operate :after ((o load-op)(c (eql (find-system "jingoh.tester"))) &key)
  (unless(featurep :bordeaux-threads)
    (warn "JINGOH: TIMEOUT is not work due to BORDEAUX-THREADS is not featured.")))

;;; These forms below are added by JINGOH.GENERATOR.
;; Ensure in ASDF for pretty printings.
(in-package :asdf)
;; Enable testing via (asdf:test-system "jingoh.tester").
(defmethod component-depends-on
           ((o test-op) (c (eql (find-system "jingoh.tester"))))
  (append (call-next-method) '((test-op "jingoh.tester.test"))))
;; Enable passing parameter for JINGOH:EXAMINER via ASDF:TEST-SYSTEM.
(defmethod operate :around
           ((o test-op) (c (eql (find-system "jingoh.tester")))
            &rest keys
            &key ((:compile-print *compile-print*))
            ((:compile-verbose *compile-verbose*)) &allow-other-keys)
  (flet ((jingoh.args (keys)
           (loop :for (key value) :on keys :by #'cddr
                 :when (find key '(:on-fails :subject :vivid) :test #'eq)
                 :collect key
                 :and
                 :collect value :else
                 :when (eq :jingoh.verbose key)
                 :collect :verbose
                 :and
                 :collect value)))
    (let ((args (jingoh.args keys)))
      (declare (special args))
      (call-next-method))))
;; Enable importing spec documentations.
(let ((system (find-system "jingoh.documentizer" nil)))
  (when system
    (load-system system)
    (defmethod perform :after
               ((o load-op) (c (eql (find-system "jingoh.tester"))))
      (with-muffled-conditions (*uninteresting-conditions*)
        (handler-case (symbol-call :jingoh.documentizer :import c)
                      (error (condition)
                             (warn "Fails to import documentation of ~S.~%~A"
                                   (coerce-name c)
                                   (princ-to-string condition))))))))
