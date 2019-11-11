; vim: ft=lisp et
(in-package :asdf)
(defsystem :jingoh.generator
  :version "0.18.7"
  :author "SATO Shinichi"
  :license "MIT"
  :description "Jingoh extension: Project skelton and test template generator."
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

(defmethod operate :after ((o load-op)(c (eql (find-system "jingoh.generator")))&key)
  (unless(fboundp(find-symbol "FUNCTION-INFORMATION" "CLTL2"))
    (warn "TRIVIAL-CLTL2 does not support ~A so JINGOH.GENERATOR can not provide full feature."
          (lisp-implementation-type))))

;;; These forms below are added by JINGOH.GENERATOR.
;; Ensure in ASDF for pretty printings.
(in-package :asdf)
;; Enable testing via (asdf:test-system "jingoh.generator").
(defmethod component-depends-on
           ((o test-op) (c (eql (find-system "jingoh.generator"))))
  (append (call-next-method) '((test-op "jingoh.generator.test"))))
;; Enable passing parameter for JINGOH:EXAMINER via ASDF:TEST-SYSTEM.
(defmethod operate :around
           ((o test-op) (c (eql (find-system "jingoh.generator")))
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
               ((o load-op) (c (eql (find-system "resignal-bind"))))
      (symbol-call :jingoh.documentizer :import c))))
