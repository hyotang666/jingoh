(in-package :jingoh.tester)

(defun check(requirement)
  #.(Doc :jingoh.tester "doc/check.F.md")
  (funcall(coerce requirement 'function)))

(defmacro defspec(&body body)
  #.(Doc :jingoh.tester "doc/defspec.M.md")
  `(PROGN (ADD-REQUIREMENT ,(apply #'make-requirement (append body *options*)))
	  *SUBJECT*))

(defmacro ? (&body body)
  #.(Doc :jingoh.tester "doc/Q.M.md")
  `(check ,(apply #'make-requirement body)))

(define-condition unsatisfied(error)
  ((test-form :initarg :test-form :reader test-form)
   (args :initarg :args :reader args)))

(defmacro & (&body body)
  #.(Doc :jingoh.tester "doc/&.M.md")
  `(PROGN ,@(mapcar(lambda(form)
		     `(ASSERT,form()
			'UNSATISFIED :TEST-FORM ',form
			,@(when(and (consp form)
				    (function-designator-p (car form)))
			    `(:ARGS (LIST ,@(cdr form))))))
	      body)
       T))

(defgeneric make-requirement(form key expected &rest params)
  (:documentation #.(Doc :jingoh.tester "doc/make-requirement.G.md")))

(defun the-push-instance-form (place type test-form expected actual position &rest options)
  #.(Doc :jingoh.tester "doc/the-push-instance-form.F.md")
  `(PUSH (MAKE-INSTANCE ',type
			:FORM ,test-form
			:EXPECTED ',expected
			:ACTUAL ,actual
			:POSITION ,position
			,@options)
	 ,place))

(defun the-standard-handling-form(result parameters test-form expected &rest body)
  #.(Doc :jingoh.tester "doc/the-standard-handling-form.F.md")
  (alexandria:with-unique-names(output)
    `(LAMBDA()
       (LET(,result (,output ""))
	 (HANDLER-CASE (SETF ,output(WITH-OUTPUT-TO-STRING(*TERMINAL-IO*)
				      ,@(let((it (getf parameters :ignore-warining)))
					  (if(and it(subtypep it 'warning))
					    `((HANDLER-BIND((,it #'MUFFLE-WARNING))
						,@body))
					    body))))
	   ,@(unless(ignore-signals 'warning parameters)
	       `((WARNING(CONDITION)
		   ,(the-push-instance-form result 'WARNING-WAS-SIGNALED `',test-form expected 'CONDITION (getf parameters :position):message `(PRINC-TO-STRING CONDITION)))))
	   ,@(unless(ignore-signals 'error parameters)
	       `((ERROR(CONDITION)
		   ,(the-push-instance-form result 'ERROR-WAS-SIGNALED `',test-form expected 'CONDITION (getf parameters :position):message `(PRINC-TO-STRING CONDITION))))))
	 (UNLESS(STRING= "" ,output)
	   ,(the-push-instance-form result 'UNEXPECTED-OUTPUT `',test-form "" output (getf parameters :position)))
	 ,result))))

(defmethod make-requirement(test-form (key(eql '=>)) expected
				      &rest parameters)
  (declare(ignore key))
  (alexandria:with-unique-names(actual result)
    (let((test(encallable(getf parameters :test #'eql)))
	 (form(canonicalize test-form parameters)))
      (the-standard-handling-form result parameters test-form expected
        `(LET((,actual ,form))
	   (UNLESS(,test ,actual ',expected)
	     ,(the-push-instance-form result 'TEST-ISSUE `',test-form expected actual (getf parameters :position):test `',test)))))))

(defmethod make-requirement(test-form (key(eql :signals)) expected
				      &rest parameters)
  (declare(ignore key))
  (alexandria:with-unique-names(actual result end output)
    (let((form(canonicalize test-form parameters)) )
      (labels((may-bind(type)
		(unless(eq type expected)
		  `((,type(LAMBDA(CONDITION)
			    ,(the-push-instance-form result (intern(format nil "~A-WAS-SIGNALED"type)) `',test-form expected 'CONDITION (getf parameters :position) :MESSAGE `(PRINC-TO-STRING CONDITION))
			    ,(ecase type
			       (WARNING `(WHEN(FIND-RESTART 'MUFFLE-WARNING CONDITION)
					   (MUFFLE-WARNING CONDITION)))
			       (ERROR `(go ,end))))))))
	      (restart-checker()
		`(LAMBDA(CONDITION)
		   (DECLARE(IGNORE CONDITION))
		   ,@(let((restarts(getf parameters :with-restarts)))
		       (when restarts
			 `((LET((,actual(MAPCAR #'FIND-RESTART ',(uiop:ensure-list restarts))))
			     (WHEN(SOME #'NULL ,actual)
			       ,(the-push-instance-form result 'MISSING-RESTARTS `',test-form restarts `(REMOVE NIL ,actual) (getf parameters :position)))))))
		   (GO ,end)))
	      )
	`(LAMBDA()
	   (PROG(,result ,actual (,output ""))
	     (HANDLER-BIND((,expected ,(restart-checker))
			   ,@(may-bind 'warning)
			   ,@(may-bind 'error))
	       (LET((*ERROR-OUTPUT*(MAKE-BROADCAST-STREAM)))
		 (SETF ,output (WITH-OUTPUT-TO-STRING(*TERMINAL-IO*)
				 (SETF ,actual (FUNCALL(COERCE '(LAMBDA(),form)
							       'FUNCTION))))))
	       ,(the-push-instance-form result 'UNEXPECTED-SUCCESS `',test-form expected actual(getf parameters :position)))
	     ,end
	     (WHEN(AND ,output (NOT(STRING= "" ,output)))
	       ,(the-push-instance-form result 'UNEXPECTED-OUTPUT `',test-form "" output (getf parameters :position)))
	     (RETURN ,result)))))))

(defmethod make-requirement(test-form (key (eql :invokes-debugger))
				      (expected null)
				      &rest parameters)
  (declare(ignore key expected))
  (alexandria:with-unique-names(result output end temp)
    (let((form(canonicalize test-form parameters)))
      `(LAMBDA()
	 (PROG(*DEBUGGER-HOOK* ,result (,output "") ,temp)
	   ;; In order to make tag visible from hook,
	   ;; we need to set hook in body.
	   (FLET((HOOK(CONDITION FUNCTION)
		   (DECLARE(IGNORE FUNCTION))
		   (WHEN(EQ CONDITION ,temp)
		     ,(the-push-instance-form result 'DEBUGGER-WAS-INVOKED `',test-form NIL 'CONDITION (getf parameters :position) :MESSAGE`(PRINC-TO-STRING CONDITION)))
		   (GO ,end))
		 (HANDLER(CONDITION)
		   (IF(FIND-RESTART 'MUFFLE-WARNING CONDITION)
		     (PROGN ,@(unless(ignore-signals 'warning parameters)
				`(,(the-push-instance-form result 'WARNING-WAS-SIGNALED `',test-form NIL 'CONDITION (getf parameters :position) :MESSAGE `(PRINC-TO-STRING CONDITION))))
			    (MUFFLE-WARNING CONDITION))
		     (SETF ,temp CONDITION)))
		 )
	     (SETF *DEBUGGER-HOOK* #'HOOK
		   ,output (WITH-OUTPUT-TO-STRING(*TERMINAL-IO*)
			     (HANDLER-BIND((WARNING #'HANDLER))
			       ,form))))
	   (WHEN(AND ,output (NOT(STRING= "" ,output)))
	     ,(the-push-instance-form result 'UNEXPECTED-OUTPUT `',test-form "" output (getf parameters :position)))
	   ,end
	   (RETURN ,result))))))

(defmethod make-requirement(test-form (key (eql :invokes-debugger))
				      (expected (eql 'not))
				      &rest parameters)
  (apply #'make-requirement test-form key NIL parameters))

(defmethod make-requirement(test-form (key(eql :invokes-debugger))
				      expected &rest parameters)
  (declare(ignore key))
  (alexandria:with-unique-names(actual result output end)
    (let((form(canonicalize test-form parameters)))
      `(LAMBDA()
	 (PROG(*DEBUGGER-HOOK* ,actual ,result (,output ""))
	   ;; In order to make tag visible from hook,
	   ;; we need to set hook in body.
	   (FLET((HOOK(CONDITION FUNCTION)
		   (DECLARE(IGNORE FUNCTION))
		   (IF(TYPEP CONDITION ',expected)
		     ,(let((restarts(getf parameters :with-restarts)))
			(when restarts
			  `(LET((,actual(MAPCAR #'FIND-RESTART ',(uiop:ensure-list restarts))))
			     (WHEN(SOME #'NULL ,actual)
			       ,(the-push-instance-form result 'MISSING-RESTARTS `',test-form restarts `(REMOVE NIL ,actual) (getf parameters :position))))))
		     ,(the-push-instance-form result 'UNMATCH-CONDITION `',test-form expected 'CONDITION (getf parameters :position) :MESSAGE `(PRINC-TO-STRING CONDITION)))
		   (GO ,end))
		 (HANDLER(CONDITION)
		   (WHEN(FIND-RESTART 'MUFFLE-WARNING CONDITION)
		     ,@(unless(ignore-signals 'warning parameters)
			 `(,(the-push-instance-form result 'WARNING-WAS-SIGNALED `',test-form expected 'CONDITION (getf parameters :position) :MESSAGE `(PRINC-TO-STRING CONDITION))))
		     (MUFFLE-WARNING CONDITION)))
		 )
	     (SETF *DEBUGGER-HOOK* #'HOOK
		   ,output (WITH-OUTPUT-TO-STRING(*TERMINAL-IO*)
			     (SETF ,actual(HANDLER-BIND((WARNING #'HANDLER))
					    ,form)))))
	   ,(the-push-instance-form result 'UNEXPECTED-SUCCESS `',test-form expected actual (getf parameters :position))
	   (WHEN(AND ,output (NOT(STRING= "" ,output)))
	     ,(the-push-instance-form result 'UNEXPECTED-OUTPUT `',test-form "" output (getf parameters :position)))
	   ,end
	   (RETURN ,result))))))

(defmethod make-requirement(test-form (key(eql :values))expected
				      &rest parameters)
  (declare(ignore key))
  (alexandria:with-unique-names(actual result)
    (let((test(encallable(getf parameters :test #'equal)))
	 (form(canonicalize test-form parameters)))
      (the-standard-handling-form result parameters test-form expected
	 `(LET((,actual(MULTIPLE-VALUE-LIST ,form)))
	    (UNLESS(,test ,actual ',expected)
	      ,(the-push-instance-form result 'ISSUE-OF-MULTIPLE-VALUES `',test-form expected actual (getf parameters :position):TEST `',test)))))))

(defmethod make-requirement(test-form (key(eql :outputs)) expected
				      &rest parameters)
  (declare(ignore key))
  (alexandria:with-unique-names(actual result)
    (let((test(encallable(getf parameters :test #'string=)))
	 (form(canonicalize test-form parameters)))
      (the-standard-handling-form result parameters test-form expected
        `(LET((,actual(WITH-OUTPUT-TO-STRING(,(getf parameters :stream '*standard-output*))
			,form)))
	   (UNLESS(,test ,expected ,actual)
	     ,(the-push-instance-form result 'WRONG-FORMAT `',test-form expected actual (getf parameters :position):TEST `',test)))))))

(defmethod make-requirement(test-form(key(eql :satisfies))expected
			     &rest parameters)
  (declare(ignore key))
  (let((actual(gensym"ACTUAL"))
       (test(encallable expected))
       (form(canonicalize test-form parameters))
       (result(gensym "RESULT")))
    (the-standard-handling-form result parameters test-form expected
      `(LET((,actual ,form))
	 (HANDLER-CASE(UNLESS(,test ,actual)
			,(the-push-instance-form result 'ISSUE `',test-form `(SATISFIES ,test) NIL (getf parameters :position)))
	   (UNSATISFIED(CONDITION)
	     ,(the-push-instance-form result 'UNSATISFIED-CLAUSE `(TEST-FORM CONDITION) T NIL (getf parameters :position):ARGS `(ARGS CONDITION))))))))

(defmethod no-applicable-method((gf(eql #'make-requirement))&rest args)
  (error'syntax-error
    :format-control "~S: Key must one of ~S but ~S~%~S"
    :format-arguments (list '?
			    (reserved-keywords gf)
			    (second args)
			    (cons '? args))))

;; These 2 vars are treated as constant.
;; but if defined with defconstant, does not work correctly in sbcl.
(defvar unspecified '#:UNSPECIFIED
  #.(Doc :jingoh.tester "doc/unspecified.V.md"))
(defvar implementation-dependent '#:IMPLEMENTATION-DEPENDENT
  #.(Doc :jingoh.tester "doc/implementation-dependent.V.md"))

(defmethod make-requirement(test-form(key(eql '=>))(expected(eql unspecified))&rest parameters)
  (declare(ignore test-form key expected parameters))
  `(CONSTANTLY NIL))

(defmethod make-requirement(test-form(key(eql '=>))
			     (expected(eql implementation-dependent))
			     &rest parameters)
  (declare(ignore key))
  (let((result(gensym "RESULT")))
    (the-standard-handling-form result parameters test-form expected
      (canonicalize test-form parameters))))

(defmethod make-requirement(test-form(key(eql :multiple-value-satisfies))
			     expected &rest parameters)
  (declare(ignore key))
  (let((actual(gensym "ACTUAL"))
       (test(encallable expected t))
       (form(canonicalize test-form parameters))
       (result(gensym"RESULT")))
    (the-standard-handling-form result parameters test-form expected
      `(LET((,actual(MULTIPLE-VALUE-LIST ,form)))
	 (UNLESS(APPLY ,test ,actual)
	   ,(the-push-instance-form result 'ISSUE-OF-MULTIPLE-VALUES `',test-form expected actual(getf parameters :position)))))))

(defmethod make-requirement(test-form(key(eql :be-the))
			     expected &rest parameters)
  (declare(ignore key))
  (let((form(canonicalize test-form parameters)))
    (alexandria:with-unique-names(actual result)
      (the-standard-handling-form result parameters test-form expected
        `(LET((,actual ,form))
	   (UNLESS(TYPEP ,actual ',expected)
	     ,(the-push-instance-form result 'ISSUE `',test-form expected `(LIST 'THE (type-of ,actual) ,actual) (getf parameters :position))))))))

(defmethod make-requirement(test-form(key(eql :equivalents))
			     expected &rest parameters)
  (declare(ignore key))
  (let((form1(canonicalize test-form parameters))
       (form2(canonicalize expected parameters))
       (test(encallable(getf parameters :test #'eql))))
    (alexandria:with-unique-names(actual1 actual2 result)
      (the-standard-handling-form result parameters test-form expected
      `(LET((,actual1 ,form1)
	    (,actual2 ,form2))
	 (UNLESS(,test ,actual1 ,actual2)
	   ,(the-push-instance-form result 'ISSUE `(LIST ',test ',test-form ',expected)T `(LIST ',test ,actual1 ,actual2)(getf parameters :position))))))))


(defmethod make-requirement(test-form(key (eql :expanded-to))
			     expected &rest parameters)
  (declare(ignore key))
  (alexandria:with-unique-names(result actual)
    (the-standard-handling-form result parameters test-form expected
      `(LET((,actual(MACROEXPAND-1 ',test-form)))
	 (UNLESS(SEXP= ,actual ',expected)
	   ,(the-push-instance-form result 'ISSUE `',test-form expected actual (getf parameters :position)))))))

(defmethod make-requirement(test-form (key(eql :output-satisfies)) expected
				      &rest parameters)
  (declare(ignore key))
  (alexandria:with-unique-names(actual result)
    (let((test(encallable expected))
	 (form(canonicalize test-form parameters)))
      (the-standard-handling-form result parameters test-form expected
        `(LET((,actual(WITH-OUTPUT-TO-STRING(,(getf parameters :stream '*standard-output*))
			,form)))
	   (HANDLER-CASE(UNLESS(,test ,actual)
			  ,(the-push-instance-form result 'ISSUE `',test-form `(SATISFIES ,test) NIL (getf parameters :position)))
	     (UNSATISFIED(CONDITION)
	       ,(the-push-instance-form result 'UNSATISFIED-CLAUSE `(TEST-FORM CONDITION) T NIL (getf parameters :position) :ARGS `(ARGS CONDITION)))))))))
