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

(defmacro & (&body body)
  #.(Doc :jingoh.tester "doc/&.M.md")
  `(OR ,@(mapcar(lambda(form)
		   `(ASSERT ,form))
	    body)
       T))

(defgeneric make-requirement(form key expected &rest params)
  (:documentation #.(Doc :jingoh.tester "doc/make-requirement.G.md")))

(defun the-push-instance-form (place type test-form expected actual position &rest options)
  #.(Doc :jingoh.tester "doc/the-push-instance-form.F.md")
  `(PUSH (MAKE-INSTANCE ',type
			:FORM ',test-form
			:EXPECTED ',expected
			:ACTUAL ,actual
			:POSITION ,position
			,@options)
	 ,place))

(defun the-standard-handling-form(result parameters test-form expected &rest body)
  #.(Doc :jingoh.tester "doc/the-standard-handling-form.F.md")
  (let((output(gensym "OUTPUT")))
    `(LAMBDA()
       (PROG(,result ,output)
	 (HANDLER-BIND((WARNING(LAMBDA(CONDITION)
				 (DECLARE(IGNORABLE CONDITION))
				 (UNLESS,(getf parameters :ignore-warning)
				   ,(the-push-instance-form result 'WARNING-WAS-SIGNALED test-form expected 'CONDITION (getf parameters :position):message `(PRINC-TO-STRING CONDITION)))
				 (GO :END)))
		       (ERROR(LAMBDA(CONDITION)
			       ,(the-push-instance-form result 'ERROR-WAS-SIGNALED test-form expected 'CONDITION (getf parameters :position):message `(PRINC-TO-STRING CONDITION))
			       (GO :END))))
	   (UNWIND-PROTECT(SETF ,output
				(WITH-OUTPUT-TO-STRING(*TERMINAL-IO*)
				  (PROGN ,@(option-form :before parameters)
					 ,@body)))
	     ,(option-form :after parameters)))
	 (UNLESS(STRING= "" ,output)
	   ,(the-push-instance-form result 'UNEXPECTED-OUTPUT test-form "" output (getf parameters :position)))
	 :END
	 (RETURN ,result)))))

(defun option-form (key parameters)
  (let((result(getf parameters key)))
    (when result
      (list result))))

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
       (result(gensym "RESULT"))
       (end(gensym "END"))
       (output(gensym "OUTPUT")))
    (labels((may-bind-warning()
	      (unless(eq 'warning expected)
		`((WARNING(LAMBDA(CONDITION)
			    ,(the-push-instance-form result 'WARNING-WAS-SIGNALED test-form expected 'CONDITION (getf parameters :position):MESSAGE `(PRINC-TO-STRING CONDITION))
			    (GO ,end))))))
	    (may-bind-error()
	      (unless(eq 'error expected)
		`((ERROR(LAMBDA(CONDITION)
			  ,(the-push-instance-form result 'ERROR-WAS-SIGNALED test-form expected 'CONDITION (getf parameters :position):MESSAGE `(PRINC-TO-STRING CONDITION))
			  (GO ,end))))))
	    (restart-checker()
	      `(LAMBDA(CONDITION)
		 (DECLARE(IGNORE CONDITION))
		 ,@(let((restarts(getf parameters :with-restarts)))
		     (when restarts
		       `((LET((,actual(MAPCAR #'FIND-RESTART ',(uiop:ensure-list restarts))))
			   (WHEN(SOME #'NULL ,actual)
			     ,(the-push-instance-form result 'MISSING-RESTARTS test-form restarts `(REMOVE NIL ,actual) (getf parameters :position)))))))
		 (GO ,end)))
	    )
      `(LAMBDA()
	 (UNWIND-PROTECT
	   (PROG(,result ,actual ,output)
	     (HANDLER-BIND((,expected ,(restart-checker))
			   ,@(may-bind-warning)
			   ,@(may-bind-error))
	       (SETF ,output (WITH-OUTPUT-TO-STRING(*TERMINAL-IO*)
			       (SETF ,actual (PROGN ,@(option-form :before parameters)
						    ,form))))
	       ,(the-push-instance-form result 'UNEXPECTED-SUCCESS test-form expected actual(getf parameters :position)))
	       ,(option-form :after parameters)
	       (UNLESS(STRING= "" ,output)
		 ,(the-push-instance-form result 'UNEXPECTED-OUTPUT test-form "" output (getf parameters :position)))
	       ,end
	       (RETURN ,result)))))))

(defmethod make-requirement(test-form (key(eql :invoke-debugger-with))
				      expected &rest parameters)
  (declare(ignore key))
  (let((actual(gensym "ACTUAL"))
       (form(canonicalize test-form parameters))
       (result(gensym "RESULT"))
       (output(gensym "OUTPUT"))
       (temp(gensym "TEMP")))
    `(LAMBDA()
       (UNWIND-PROTECT
	 (PROG(*DEBUGGER-HOOK* ,output ,actual ,result ,temp)
	   ;; In order to make tag visible from hook,
	   ;; we need to set hook in body.
	   (SETF *DEBUGGER-HOOK*(LAMBDA(CONDITION FUNCTION)
				  (DECLARE(IGNORE CONDITION FUNCTION))
				  (POP ,temp)
				  (GO :END))
		 ,output (SETF ,actual (HANDLER-BIND((WARNING(LAMBDA(CONDITION)
							       (UNLESS,(getf parameters :ignore-warning)
								 ,(the-push-instance-form temp 'WARNING-WAS-SIGNALED test-form expected 'CONDITION (getf parameters :position):MESSAGE `(PRINC-TO-STRING CONDITION)))
							       (GO :END))))
					 ,@(option-form :before parameters)
					 ,form)))
	   ,(the-push-instance-form result 'UNEXPECTED-SUCCESS test-form expected actual (getf parameters :position))
	   (UNLESS(STRING= "" ,output)
	     ,(the-push-instance-form result 'UNEXPECTED-OUTPUT test-form "" output (getf parameters :position)))
	   :END
	   (RETURN(APPEND ,result ,temp)))
	 ,(option-form :after parameters)))))

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
  #.(Doc :jingoh.tester "doc/unspecified.V.md"))
(defvar implementation-dependent '#:IMPLEMENTATION-DEPENDENT
  #.(Doc :jingoh.tester "doc/implementation-dependent.V.md"))

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
  (let((output(gensym "OUTPUT"))
       (result(gensym "RESULT")))
    `(LAMBDA()
       (UNWIND-PROTECT
	 (PROG(*DEBUGGER-HOOK* ,result ,output)
	   ;; In order to make tag visible from hook,
	   ;; we need to set hook in body.
	   (SETF *DEBUGGER-HOOK*(LAMBDA(CONDITION HOOK)
				  (DECLARE(IGNORE HOOK))
				  ,(the-push-instance-form result 'debugger-was-invoked test-form NIL 'CONDITION (getf parameters :position):message `(PRINC-TO-STRING CONDITION))
				  (GO :END)))
	   (SETF ,output (WITH-OUTPUT-TO-STRING(*TERMINAL-IO*)
			   ,@(option-form :before parameters)
			   ,(canonicalize test-form parameters)))
	   (UNLESS(STRING= "" ,output)
	     ,(the-push-instance-form result 'UNEXPECTED-OUTPUT test-form "" output (getf parameters :position)))
	   :END
	   (RETURN ,result))
	 ,(option-form :after parameters)))))
