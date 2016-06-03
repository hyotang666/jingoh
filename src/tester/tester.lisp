(in-package :jingoh.tester)

(defun check(requirement)
  #.(doc :jingoh.tester "doc/tester/check.F.md")
  (funcall(coerce requirement 'function)))

(defmacro defspec(&body body)
  #.(doc :jingoh.tester "doc/tester/defspec.M.md")
  `(PROGN (ADD-REQUIREMENT ,(apply #'make-requirement body))
	  *SUBJECT*))

(defmacro ? (&body body)
  #.(doc :jingoh.tester "doc/tester/Q.M.md")
  `(check ,(apply #'make-requirement body)))

(defmacro & (&body body)
  #.(doc :jingoh.tester "doc/tester/&.M.md")
  `(OR ,@(mapcar(lambda(form)
		   `(ASSERT ,form))
	    body)
       T))

(defgeneric make-requirement(form key expected &rest params)
  (:documentation #.(doc :jingoh.tester "doc/tester/make-requirement.G.md")))

(defun the-push-instance-form (place type test-form expected actual position &rest options)
  #.(doc :jingoh.tester "doc/tester/the-push-instance-form.F.md")
  `(PUSH (MAKE-INSTANCE ',type
			:FORM ',test-form
			:EXPECTED ',expected
			:ACTUAL ,actual
			:POSITION ,position
			,@options)
	 ,place))

(defun the-standard-handling-form(result parameters test-form expected &rest body)
  #.(doc :jingoh.tester "doc/tester/the-standard-handling-form.F.md")
  `(LAMBDA()
     (PROG(,result)
       (HANDLER-BIND((WARNING(LAMBDA(CONDITION)
			       (DECLARE(IGNORABLE CONDITION))
			       (UNLESS,(getf parameters :ignore-warning)
				 ,(the-push-instance-form result 'WARNING-WAS-SIGNALED test-form expected 'CONDITION (getf parameters :position):message `(PRINC-TO-STRING CONDITION)))))
		     (ERROR(LAMBDA(CONDITION)
			     ,(the-push-instance-form result 'ERROR-WAS-SIGNALED test-form expected 'CONDITION (getf parameters :position):message `(PRINC-TO-STRING CONDITION))
			     (GO :END))))
	 ,@body)
       :END
       (RETURN ,result))))

(defmethod make-requirement(test-form (key(eql '=>)) expected
				      &rest parameters)
  (declare(ignore key))
  (let((actual(gensym"ACTUAL"))
       (test(encallable(getf parameters :test #'eql)))
       (form(canonicalize test-form parameters))
       (result(gensym "RESULT")))
    (the-standard-handling-form result parameters test-form expected
      `(LET((,actual ,form))
	 (UNLESS(,test ,actual ',expected)
	   ,(the-push-instance-form result 'ISSUE test-form expected actual (getf parameters :position):test `',test))))))

(defmethod make-requirement(test-form (key(eql :signals)) expected
				      &rest parameters)
  (declare(ignore key))
  (let((actual(gensym "ACTUAL"))
       (form(canonicalize test-form parameters))
       (result(gensym "RESULT")))
    `(LAMBDA()
       (PROG(,result)
	 (HANDLER-CASE,form
	   (,expected()NIL) ; do nothing.
	   ,@(unless(eq 'warning expected)
	       `((WARNING(CONDITION)
		   ,(the-push-instance-form result 'WARNING-WAS-SIGNALED test-form expected 'CONDITION (getf parameters :position):MESSAGE `(PRINC-TO-STRING CONDITION)))))
	   ,@(unless(eq 'error expected)
	       `((ERROR(CONDITION)
		   ,(the-push-instance-form result 'ERROR-WAS-SIGNALED test-form expected 'CONDITION (getf parameters :position):MESSAGE `(PRINC-TO-STRING CONDITION)))))
	   (:NO-ERROR(&REST ,actual)
	     ,(the-push-instance-form result 'UNEXPECTED-SUCCESS test-form expected ``(VALUES ,@,actual)(getf parameters :position))))
	 (RETURN ,result)))))

(defmethod make-requirement(test-form (key(eql :invoke-debugger-with))
				      expected &rest parameters)
  (declare(ignore key))
  (let((actual(gensym "ACTUAL"))
       (form(canonicalize test-form parameters))
       (result(gensym "RESULT"))
       (temp(gensym "TEMP")))
    `(LAMBDA()
       (PROG(*DEBUGGER-HOOK* ,actual ,result ,temp)
	 ;; In order to make tag visible from hook,
	 ;; we need to set hook in body.
	 (SETF *DEBUGGER-HOOK*(LAMBDA(CONDITION FUNCTION)
				(DECLARE(IGNORE CONDITION FUNCTION))
				(POP ,temp)
				(GO :END))
	       ,actual (HANDLER-BIND((WARNING(LAMBDA(CONDITION)
					       (UNLESS,(getf parameters :ignore-warning)
						 ,(the-push-instance-form temp 'WARNING-WAS-SIGNALED test-form expected 'CONDITION (getf parameters :position):MESSAGE `(PRINC-TO-STRING CONDITION))))))
			 ,form))
	 ,(the-push-instance-form result 'UNEXPECTED-SUCCESS test-form expected actual (getf parameters :position))
	 :END
	 (RETURN(APPEND ,result ,temp))))))

(defmethod make-requirement(test-form (key(eql :values))expected
				      &rest parameters)
  (declare(ignore key))
  (let((actual(gensym "ACTUAL"))
       (test(encallable(getf parameters :test #'equal)))
       (form(canonicalize test-form parameters))
       (result(gensym "RESULT")))
    (the-standard-handling-form result parameters test-form expected
      `(LET((,actual(MULTIPLE-VALUE-LIST ,form)))
	 (UNLESS(,test ,actual ',expected)
	   ,(the-push-instance-form result 'ISSUE-OF-MULTIPLE-VALUES test-form expected actual (getf parameters :position):TEST `',test))))))

(defmethod make-requirement(test-form (key(eql :output)) expected
				      &rest parameters)
  (declare(ignore key))
  (let((actual(gensym "ACTUAL"))
       (test(encallable(getf parameters :test #'string=)))
       (form(canonicalize test-form parameters))
       (result(gensym "RESULT")))
    (the-standard-handling-form result parameters test-form expected
      `(LET((,actual(WITH-OUTPUT-TO-STRING(,(getf parameters :stream '*standard-output*))
		      ,form)))
	 (UNLESS(,test ,expected ,actual)
	   ,(the-push-instance-form result 'WRONG-FORMAT test-form expected actual (getf parameters :position):TEST `',test))))))

(defmethod make-requirement(test-form(key(eql :satisfies))expected
			     &rest parameters)
  (declare(ignore key))
  (let((actual(gensym"ACTUAL"))
       (test(encallable expected))
       (form(canonicalize test-form parameters))
       (result(gensym "RESULT")))
    (the-standard-handling-form result parameters test-form expected
      `(LET((,actual ,form))
	 (UNLESS(,test ,actual)
	   ,(the-push-instance-form result 'ISSUE test-form NIL actual(getf parameters :position):TEST `',test))))))

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
  #.(doc :jingoh.tester "doc/tester/unspecified.V.md"))
(defvar implementation-dependent '#:IMPLEMENTATION-DEPENDENT
  #.(doc :jingoh.tester "doc/tester/implementation-dependent.V.md"))

(defmethod make-requirement(test-form(key(eql '=>))(expected(eql unspecified))&rest parameters)
  (declare(ignore key))
  (when(getf parameters :from-id)
    (setf expected implementation-dependent))
  (let((result(gensym "RESULT")))
    (the-standard-handling-form result parameters test-form expected
      (canonicalize test-form parameters))))

(defmethod make-requirement(test-form(key(eql '=>))
			     (expected(eql implementation-dependent))
			     &rest args)
  (apply #'make-requirement test-form key unspecified(list* :from-id t args)))

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
	   ,(the-push-instance-form result 'ISSUE-OF-MULTIPLE-VALUES test-form expected actual(getf parameters :position)))))))

(defmethod make-requirement(test-form(key(eql :never-invoke-debugger))expected &rest parameters)
  (declare(ignore key expected))
  (let((result(gensym "RESULT")))
    `(LAMBDA()
       (PROG(*DEBUGGER-HOOK* ,result)
	 ;; In order to make tag visible from hook,
	 ;; we need to set hook in body.
	 (SETF *DEBUGGER-HOOK*(LAMBDA(CONDITION HOOK)
				(DECLARE(IGNORE HOOK))
				,(the-push-instance-form result 'debugger-was-invoked test-form NIL 'CONDITION (getf parameters :position):message `(PRINC-TO-STRING CONDITION))
				(GO :END)))
	 ,(canonicalize test-form parameters)
	 :END
	 (RETURN ,result)))))
