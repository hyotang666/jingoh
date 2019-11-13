; vim: ft=lisp et
(in-package :asdf)

(defsystem :jingoh.org
  :version "0.1.10"
  :description "Jingoh's background database system"
  :author "SATO Shinichi"
  :license "MIT"
  :long-description #.(uiop:read-file-string
                        (uiop:subpathname *load-pathname* "CONCEPTS.md"))
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

;;; These forms below are added by JINGOH.GENERATOR.
;; Ensure in ASDF for pretty printings.
(in-package :asdf)
;; Enable testing via (asdf:test-system "jingoh.org").
(defmethod component-depends-on
           ((o test-op) (c (eql (find-system "jingoh.org"))))
  (append (call-next-method) '((test-op "jingoh.org.test"))))
;; Enable passing parameter for JINGOH:EXAMINER via ASDF:TEST-SYSTEM.
(defmethod operate :around
           ((o test-op) (c (eql (find-system "jingoh.org")))
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
  (when (and system (not (featurep :clisp)))
    (load-system system)
    (defmethod perform :after
               ((o load-op) (c (eql (find-system "jingoh.org"))))
      (symbol-call :jingoh.documentizer :import c))))
