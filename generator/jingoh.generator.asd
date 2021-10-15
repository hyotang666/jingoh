; vim: ft=lisp et
(in-package :asdf)

(defsystem :jingoh.generator
  :version "1.2.38"
  :author "SATO Shinichi"
  :license "MIT"
  :description "Jingoh extension: Project skelton and test template generator and more."
  :in-order-to((test-op(test-op "jingoh.generator.test")))
  :depends-on (
               "alexandria"             ; Utilities.
               "uiop"                   ; Utilities, implicitly via asdf.
               "millet"                 ; Wrappter for implementation dependent utilities.
               "closer-mop"             ; Wrapper for meta object protocols.
               "trivial-cltl2"          ; Wrapper for cltl2.
               "lambda-fiddle"          ; Operators for lambda-list.
               "prompt-for"             ; Type safe user input.
               "named-readtables"       ; Readtable manager.
               "cl-unification"         ; Unification.
               "asdf"                   ; System loading.
               ;; Quicklisp project can not test to build the system that depends on quicklisp.
               ;; "quicklisp"              ; System installing.
               )
  :pathname "src"
  :components ((:file "package")
               (:file "util" :depends-on ("package"))
               (:file "symbol-generate" :depends-on ("package"))

               (:file "generate" :depends-on ("symbol-generate"))
               (:file "dribble" :depends-on ("symbol-generate" "util"))
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
  (when system
    (load-system system)
    (defmethod perform :after
               ((o load-op) (c (eql (find-system "jingoh.generator"))))
      (with-muffled-conditions (*uninteresting-conditions*)
        (handler-case (symbol-call :jingoh.documentizer :import c)
                      (error (condition)
                             (warn "Fails to import documentation of ~S.~%~A"
                                   (coerce-name c)
                                   (princ-to-string condition))))))))
