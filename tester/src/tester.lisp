(in-package :jingoh.tester)

(defun requirement-form (requirement) (apply #'make-requirement requirement))

(defun check (requirement)
  (macrolet ((with-internal-issue-handling (form)
               `(handler-case ,form
                  (error (c)
                    (lambda ()
                      (list
                        (make-instance 'jingoh-internal-issue
                                       :actual :skipped
                                       :message (princ-to-string c)
                                       :form (car requirement)
                                       :expected :do-test
                                       :line (getf (cdr requirement)
                                                   :line))))))))
    (funcall
      (with-internal-issue-handling
       (coerce (with-internal-issue-handling (requirement-form requirement))
               'function)))))

(defmacro defspec (&whole whole &body body)
  (check-bnf:check-bnf (:whole whole)
    ((body (test-form dispatch-key expected option*))
     (test-form check-bnf:expression)
     (dispatch-key
      (member =>
              :be-the :satisfies
              :outputs :values
              :multiple-value-satisfies :output-satisfies
              :expanded-to :equivalents
              :signals :invokes-debugger))
     (expected check-bnf:expression)
     (option* option-key check-bnf:expression)
     (option-key
      (member :timeout :line
              :around :before
              :after :stream
              :with-restarts :ignore-signals
              :lazy :test
              :doc-type :as))))
  `(eval-when (:load-toplevel :execute)
     ,@(unless (getf (cdddr body) :lazy '#:does-not-exist)
         `(,(canonicalize (car body) (cdddr body))))
     ,@(mapcar
         (lambda (subject)
           `(add-requirement ',subject
                             ',(let ((as (getf (org-options *org*) :as)))
                                 (if as
                                     (append
                                       (list (subst subject as (car body)))
                                       (cdr body) (org-options *org*))
                                     (append body (org-options *org*))))))
         (org-current-subjects *org*))
     (org-current-subjects *org*)))

(defmacro ? (&body body) `(check ',body))

(define-condition unsatisfied (error)
  ((test-form :initarg :test-form :reader test-form)
   (args :initform nil :initarg :args :reader args)))

(defmacro & (&body body)
  `(progn
    ,@(mapcar
        (lambda (form)
          (if (typep form '(cons (satisfies function-designator-p) t))
              (let ((vars
                     (loop :repeat (length (cdr form))
                           :collect (gensym))))
                `(let ,(mapcar #'list vars (cdr form))
                   (assert (,(car form) ,@vars) ()
                     'unsatisfied :test-form ',form
                                  :args (list ,@vars))))
              `(assert ,form () 'unsatisfied :test-form ',form)))
        body)
    t))

(defgeneric make-requirement (form key expected &rest params))

(defun the-push-instance-form
       (place type test-form expected actual line &rest options)
  `(push
    (make-instance ',type :form ,test-form :expected ',expected :actual ,actual
                   :line ,line ,@options)
    ,place))

(defun the-standard-handling-form
       (result parameters test-form expected &rest body)
  (alexandria:with-unique-names (output)
    `(lambda ()
       (let (,result (,output ""))
         (handler-case
             (setf ,output
                     (with-output-to-string (*standard-output*)
                       (with-integrated-output-stream (*standard-output*)
                         ,@(if (ignore-signals 'warning parameters)
                               `((handler-bind ((,(getf parameters
                                                        :ignore-signals)
                                                 (lambda (condition)
                                                   (when (find-restart
                                                           'muffle-warning
                                                           condition)
                                                     (muffle-warning
                                                       condition)))))
                                   ,@body))
                               body))))
           ,@(unless (ignore-signals 'warning parameters)
               `((warning (condition)
                  ,(the-push-instance-form result 'warning-was-signaled
                     `',test-form expected 'condition (getf parameters :line)
                     :message `(princ-to-string condition)))))
           ,@(unless (ignore-signals 'error parameters)
               `((error (condition)
                        ,(the-push-instance-form result 'error-was-signaled
                           `',test-form expected 'condition
                           (getf parameters :line) :message
                           `(princ-to-string condition))))))
         (unless (string= "" ,output)
           ,(the-push-instance-form result 'unexpected-output `',test-form ""
              output (getf parameters :line)))
         ,result))))

(set-pprint-dispatch
  '(cons (member the-standard-handling-form the-push-instance-form))
  (formatter "~:<~W~^ ~1I~:_~@{~W~^ ~:_~}~:>"))

(defmethod make-requirement
           (test-form (key (eql '=>)) expected &rest parameters)
  (declare (ignore key))
  (alexandria:with-unique-names (actual result)
    (let ((test (encallable (getf parameters :test #'eql)))
          (form (canonicalize test-form parameters)))
      (the-standard-handling-form result parameters test-form expected
        `(let ((,actual ,form))
           (unless (,test ,actual ',expected)
             ,(the-push-instance-form result 'test-issue `',test-form expected
                actual (getf parameters :line) :test `',test)))))))

(defmethod make-requirement
           (test-form (key (eql :signals)) expected &rest parameters)
  (declare (ignore key))
  (alexandria:with-unique-names (actual result end output)
    (let ((form (canonicalize test-form parameters)))
      (labels ((may-bind (type)
                 (unless (or (subtypep type expected)
                             (subtypep type (getf parameters :ignore-signals)))
                   `((,type
                      (lambda (condition)
                        ,(the-push-instance-form result
                           (intern (format nil "~A-WAS-SIGNALED" type))
                           `',test-form expected 'condition
                           (getf parameters :line) :message
                           `(princ-to-string condition))
                        ,(ecase type
                           (warning
                            `(when (find-restart 'muffle-warning condition)
                               (muffle-warning condition)))
                           (error `(go ,end))))))))
               (restart-checker ()
                 `(lambda (condition)
                    (declare (ignorable condition))
                    ,@(let ((restarts (getf parameters :with-restarts)))
                        (when restarts
                          `((let ((,actual
                                   (mapcar #'find-restart
                                           ',(uiop:ensure-list restarts))))
                              (when (some #'null ,actual)
                                ,(the-push-instance-form result
                                   'missing-restarts `',test-form restarts
                                   `(mapcar #'restart-name
                                            (compute-restarts condition))
                                   (getf parameters :line)))))))
                    (go ,end))))
        `(lambda ()
           (prog (,result ,actual (,output ""))
             (handler-bind ((,expected ,(restart-checker))
                            ,@(may-bind 'warning) ,@(may-bind 'error))
               (setf ,output
                       (with-output-to-string (*standard-output*)
                         (with-integrated-output-stream (*standard-output*)
                           (let ((*error-output* (make-broadcast-stream)))
                             (setf ,actual
                                     (with-compilation-unit (:override t)
                                       (funcall
                                         (coerce '(lambda () ,form)
                                                 'function))))))))
               ,(the-push-instance-form result 'unexpected-success `',test-form
                  expected actual (getf parameters :line)))
            ,end
             (when (and ,output (not (string= "" ,output)))
               ,(the-push-instance-form result 'unexpected-output `',test-form
                  "" output (getf parameters :line)))
             (return ,result)))))))

(defmethod make-requirement
           (test-form (key (eql :invokes-debugger)) (expected null)
            &rest parameters)
  (declare (ignore key expected))
  (alexandria:with-unique-names (result output end temp)
    (let ((form (canonicalize test-form parameters)))
      `(lambda ()
         (prog (*debugger-hook* ,result (,output "") ,temp)
           ;; In order to make tag visible from hook,
           ;; we need to set hook in body.
           (flet ((hook (condition function)
                    (declare (ignore function))
                    (when (eq condition ,temp)
                      ,(the-push-instance-form result 'debugger-was-invoked
                         `',test-form nil 'condition (getf parameters :line)
                         :message `(princ-to-string condition)))
                    (go ,end))
                  (handler (condition)
                    (if (find-restart 'muffle-warning condition)
                        (progn
                         ,@(unless (ignore-signals 'warning parameters)
                             `(,(the-push-instance-form result
                                  'warning-was-signaled `',test-form nil
                                  'condition (getf parameters :line) :message
                                  `(princ-to-string condition))))
                         (muffle-warning condition))
                        (setf ,temp condition))))
             (setf *debugger-hook* #'hook
                   ,output
                     (with-output-to-string (*standard-output*)
                       (with-integrated-output-stream (*standard-output*)
                         (handler-bind ((warning #'handler))
                           ,form)))))
           (when (and ,output (not (string= "" ,output)))
             ,(the-push-instance-form result 'unexpected-output `',test-form ""
                output (getf parameters :line)))
          ,end
           (return ,result))))))

(defmethod make-requirement
           (test-form (key (eql :invokes-debugger)) (expected (eql 'not))
            &rest parameters)
  (apply #'make-requirement test-form key nil parameters))

(defmethod make-requirement
           (test-form (key (eql :invokes-debugger)) expected &rest parameters)
  (declare (ignore key))
  (alexandria:with-unique-names (actual result output end)
    (let ((form (canonicalize test-form parameters))
          (test (getf parameters :test)))
      `(lambda ()
         (prog (*debugger-hook* ,actual ,result (,output ""))
           ;; In order to make tag visible from hook,
           ;; we need to set hook in body.
           (flet ((hook (condition function)
                    (declare (ignore function))
                    ,@(when test
                        `((handler-case
                              (unless (,(encallable test) condition)
                                ,(the-push-instance-form result 'test-issue
                                   `',test-form t nil (getf parameters :line)
                                   :test `',test))
                            (unsatisfied (condition)
                              ,(the-push-instance-form result
                                 'unsatisfied-clause `(test-form condition) t
                                 nil (getf parameters :line) :args
                                 `(args condition))))))
                    (if (ignore-errors (typep condition ',expected))
                        ,(let ((restarts (getf parameters :with-restarts)))
                           (when restarts
                             `(let ((,actual
                                     (mapcar #'find-restart
                                             ',(uiop:ensure-list restarts))))
                                (when (some #'null ,actual)
                                  ,(the-push-instance-form result
                                     'missing-restarts `',test-form restarts
                                     `(mapcar #'restart-name
                                              (compute-restarts condition))
                                     (getf parameters :line))))))
                        ,(the-push-instance-form result 'unmatch-condition
                           `',test-form expected 'condition
                           (getf parameters :line) :message
                           `(princ-to-string condition)))
                    (go ,end))
                  (handler (condition)
                    (when (find-restart 'muffle-warning condition)
                      ,@(unless (ignore-signals 'warning parameters)
                          `(,(the-push-instance-form result
                               'warning-was-signaled `',test-form expected
                               'condition (getf parameters :line) :message
                               `(princ-to-string condition))))
                      (muffle-warning condition))))
             (setf *debugger-hook* #'hook
                   ,output
                     (with-output-to-string (*standard-output*)
                       (with-integrated-output-stream (*standard-output*)
                         (setf ,actual
                                 (handler-bind ((warning #'handler))
                                   ,form))))))
          ,(the-push-instance-form result 'unexpected-success `',test-form
             expected actual (getf parameters :line))
           (when (and ,output (not (string= "" ,output)))
             ,(the-push-instance-form result 'unexpected-output `',test-form ""
                output (getf parameters :line)))
          ,end
           (return ,result))))))

(defmethod make-requirement
           (test-form (key (eql :values)) expected &rest parameters)
  (declare (ignore key))
  (alexandria:with-unique-names (actual result)
    (let ((test (encallable (getf parameters :test #'equal)))
          (form (canonicalize test-form parameters)))
      (the-standard-handling-form result parameters test-form expected
        `(let ((,actual (multiple-value-list ,form)))
           (unless (,test ,actual ',expected)
             ,(the-push-instance-form result 'issue-of-multiple-values
                `',test-form expected actual (getf parameters :line) :test
                `',test)))))))

(defmethod make-requirement
           (test-form (key (eql :outputs)) expected &rest parameters)
  (declare (ignore key))
  (alexandria:with-unique-names (actual result)
    (let ((test (encallable (getf parameters :test #'string=)))
          (form (canonicalize test-form parameters)))
      (the-standard-handling-form result parameters test-form expected
        `(let ((,actual
                (with-output-to-string
                    (,(getf parameters :stream '*standard-output*))
                  ,form)))
           (unless (,test ,expected ,actual)
             ,(the-push-instance-form result 'wrong-format `',test-form
                expected actual (getf parameters :line) :test `',test)))))))

(defmethod make-requirement
           (test-form (key (eql :satisfies)) expected &rest parameters)
  (declare (ignore key))
  (let ((actual (gensym "ACTUAL"))
        (test (encallable expected))
        (form (canonicalize test-form parameters))
        (result (gensym "RESULT")))
    (the-standard-handling-form result parameters test-form expected
      `(let ((,actual ,form))
         (handler-case
             (unless (,test ,actual)
               ,(the-push-instance-form result 'unsatisfied-clause `',test-form
                  `(satisfies ,test) nil (getf parameters :line) :args
                  `(list ,actual)))
           (unsatisfied (condition)
             ,(the-push-instance-form result 'unsatisfied-clause
                `(test-form condition) t nil (getf parameters :line) :args
                `(args condition))))))))

(defmethod no-applicable-method ((gf (eql #'make-requirement)) &rest args)
  (error 'syntax-error
         :format-control "~S: Key must one of ~S but ~S~%~S"
         :format-arguments (list '? (reserved-keywords gf) (second args)
                                 (cons '? args))))

(defmethod make-requirement
           (test-form (key (eql '=>)) (expected (eql 'unspecified))
            &rest parameters)
  (declare (ignore test-form key expected parameters))
  '(lambda () nil))

(defmethod make-requirement
           (test-form (key (eql '=>))
            (expected (eql 'implementation-dependent))
            &rest parameters)
  (declare (ignore key))
  (let ((result (gensym "RESULT")))
    (the-standard-handling-form result parameters test-form expected
      (canonicalize test-form parameters))))

(defmethod make-requirement
           (test-form (key (eql :multiple-value-satisfies)) expected
            &rest parameters)
  (declare (ignore key))
  (let ((actual (gensym "ACTUAL"))
        (test (encallable expected t))
        (form (canonicalize test-form parameters))
        (result (gensym "RESULT")))
    (the-standard-handling-form result parameters test-form expected
      `(let ((,actual (multiple-value-list ,form)))
         (handler-case
             (unless (apply ,test ,actual)
               ,(the-push-instance-form result 'issue-of-multiple-values
                  `',test-form expected actual (getf parameters :line)))
           (unsatisfied (condition)
             ,(the-push-instance-form result 'unsatisfied-clause
                `(test-form condition) t nil (getf parameters :line) :args
                `(args condition))))))))

(defmethod make-requirement
           (test-form (key (eql :be-the)) expected &rest parameters)
  (declare (ignore key))
  (let ((form (canonicalize test-form parameters)))
    (alexandria:with-unique-names (actual result)
      (the-standard-handling-form result parameters test-form expected
        `(let ((,actual ,form))
           (unless (typep ,actual ',expected)
             ,(the-push-instance-form result 'issue `',test-form expected
                `(list 'the (type-of ,actual) ,actual)
                (getf parameters :line))))))))

(defmethod make-requirement
           (test-form (key (eql :equivalents)) expected &rest parameters)
  (declare (ignore key))
  (let ((form1 (canonicalize test-form parameters))
        (form2 (canonicalize expected parameters))
        (test (encallable (getf parameters :test #'eql))))
    (alexandria:with-unique-names (actual1 actual2 result)
      (the-standard-handling-form result parameters test-form expected
        `(let ((,actual1 ,form1) (,actual2 ,form2))
           (unless (,test ,actual1 ,actual2)
             ,(the-push-instance-form result 'issue
                `(list ',test ',test-form ',expected) t
                `(list ',test ,actual1 ,actual2) (getf parameters :line))))))))

(defmethod make-requirement
           (test-form (key (eql :expanded-to)) expected &rest parameters)
  (declare (ignore key))
  (alexandria:with-unique-names (result actual)
    (the-standard-handling-form result parameters test-form expected
      `(let ((,actual (macroexpand-1 ',(copy-tree test-form))))
         (unless (sexp= ,actual ',expected)
           ,(the-push-instance-form result 'issue `',test-form expected actual
              (getf parameters :line)))))))

(defmethod make-requirement
           (test-form (key (eql :output-satisfies)) expected &rest parameters)
  (declare (ignore key))
  (alexandria:with-unique-names (actual result)
    (let ((test (encallable expected))
          (form (canonicalize test-form parameters)))
      (the-standard-handling-form result parameters test-form expected
        `(let ((,actual
                (with-output-to-string
                    (,(getf parameters :stream '*standard-output*))
                  ,form)))
           (handler-case
               (unless (,test ,actual)
                 ,(the-push-instance-form result 'issue `',test-form
                    `(satisfies ,test) nil (getf parameters :line)))
             (unsatisfied (condition)
               ,(the-push-instance-form result 'unsatisfied-clause
                  `(test-form condition) t nil (getf parameters :line) :args
                  `(args condition)))))))))
