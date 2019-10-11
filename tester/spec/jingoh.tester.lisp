(defpackage :jingoh.tester.spec
  (:use :cl :jingoh :jingoh.tester :jingoh.org)
  (:import-from :jingoh.tester #:*color-hook*)
  )
(in-package :jingoh.tester.spec)
(setup :jingoh.tester)

(requirements-about DEFSPEC :around (let((*org* (make-org)))
				      (call-body))
		    :doc-type function)

;;;; Description:
; Define specification.

#+syntax
(DEFSPEC &body body) ; => result

;;;; Arguments and Values:

; body := see ?

; result := list which represents current subjects.
#?(defspec (+) => 0) => (NIL)
,:test equal

;;;; Affected By:
; *org*

;;;; Side-Effects:
; Modify *ORG*'s specification slot.
#?(progn (princ (org-requirements-count *org*))
	 (defspec (+) => 0)
	 (princ (org-requirements-count *org*)))
:outputs "01"

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about & :doc-type function)

;;;; Description:
; When (cl:and ...) is failed, we are not able to get information
; about which clause is failed.
; & provides such information.
#?(& T T T) => T
#?(& T NIL T) :signals jingoh.tester::UNSATISFIED

#+syntax
(& &body body) ; => result

;;;; Arguments and Values:

; body := some forms which generates non NIL value.

; result := When all form return non NIL value, returns T.
; Otherwise error.

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:
; When one form is evaluated to NIL, short cut is occur.
#?(& T NIL (princ :hoge)) :signals jingoh.tester::UNSATISFIED

; *TIPS* - When clause is function, :args slot contains arguments.
#?(handler-case(& (eq :hoge :fuga))
    (jingoh.tester::unsatisfied(c)
      (jingoh.tester::args c)))
=> (:hoge :fuga)
,:test equal
; But when cluase is variable, macro form, or special form, 
; :args slot is stored with NIL.
#?(handler-case(& (and (eq :hoge :fuga)))
    (jingoh.tester::unsatisfied(c)
      (jingoh.tester::args c)))
=> NIL

;;;; Exceptional-Situations:

(requirements-about SEXP= :doc-type function)

;;;; Description:
; tests equalilty as syntax. This is useful to test MACROEXPENDed form.
#?(sexp= '(let((#0=#:var 0)) #0#)
	 '(let((var 0))var))
=> T
#?(sexp= '#:foo 'var) => T
#?(sexp= 'foo '#:foo) => NIL

#+syntax
(SEXP= sexp1 sexp2) ; => result

;;;; Arguments and Values:

; sexp1 := form, which may include GENSYMed symbol.

; sexp2 := form.

; result := boolean

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:
; Still alpha quality.

;;;; Exceptional-Situations:

(requirements-about ? :doc-type function)

;;;; Description:
; One shot tester.
#?(? (+) => 0) => NIL
#?(? (+) => 1) :satisfies (lambda($result)
			    (& (listp $result)
			       (= 1 (length $result))
			       (every #'issue-p $result)))

#+syntax
(? &body body) ; => result

;;;; Arguments and Values:

; body := (test-form dispatch-key expected option*)

; test-form := form which is tested.

; dispatch-key := (member => :be-the :satisfies :outputs :values :multiple-value-satisfies :output-satisfies :expanded-to :equivalents :signals :invokes-debugger)

; expected := lisp value which is expected value of test-form.

; option := key value pair.

; key := (member :test :lazy :ignore-signals :with-restarts :stream :before :after :around :line :as :timeout)

;;; dispatch-keys.

; standard usage.
#?(? (+) => 0) => NIL

; When expected value is unreadable object, use :be-the.
#?(? #'car :be-the function) => NIL

; Above code can write like below.
#?(? #'car :satisfies functionp) => NIL

; To test multiple values, use :values
#?(? (floor 2 3) :values (0 2)) => NIL

; To test output strings, use :outputs
#?(? (princ :foo) :outputs "FOO") => NIL

; To test complex multiple values, use :multiple-value-satisfies.
#?(? (values 0 *standard-output*)
     :multiple-value-satisfies (lambda(n s)
				 (& (numberp n)
				    (streamp s))))
=> NIL

; To test complex string, use :output-satisfies
; TODO

; To test condition, use :signals.
#?(? (error "error") :signals ERROR) => NIL
#?(? (signal 'warning) :signals warning) => NIL

; To test debugger is invoked or not, use :invokes-debugger
#?(? (invoke-debugger(make-condition 'error)) :invokes-debugger ERROR)
=> NIL

; To test macro expansion, use :expanded-to
#?(& T) :expanded-to (progn (assert t ()'jingoh.tester::unsatisfied
				    :test-form 'T)
			    T)

; To test two codes are semantically equal, use :equivalents
#?(loop :for i :in '(1 2 3) :sum i)
:equivalents (reduce #'+ '(1 2 3))

;;; option keys

; When expected value is not symbol, character or integer, use :test option.
#?(? (list 1 2 3) => (1 2 3) :test equal)
=> NIL

; To ignore condition especially warning, use :ignore-signals.
#?(? (progn (warn "warning")
	    t)
     => T :ignore-signals warning :stream nil)
=> NIL
,:ignore-signals warning
#?(? (signal 'warning) => NIL :ignore-signals warning)
=> NIL
,:ignore-signals warning

; To test compile time error is occur, use :lazy.
#?(? (defun "invalid"()(princ :hoge))
     :signals ERROR :lazy T)
=> NIL

; To test restart is available, use :with-restarts with :signals.
#?(? (warn "warning") :signals warning :with-restarts muffle-warning)
=> NIL

; When dispatch key is :outputs, you can specify stream by :stream.
#?(? (princ :foo *error-output*) :outputs "FOO" :stream *error-output*)
=> NIL

; When test needs setup, you can use :before.
#?(? (princ :foo) :outputs "HOGEFOO" :before (princ :hoge))
=> NIL

; When test needs teardown, you can use :after.
#?(? (princ :foo) :outputs "FOOBAR" :after (princ :bar))
=> NIL

; *NOTE!* -  Internally, :after is placed as clean up form of CL:UNWIND-PROTECT, so :after's return value is discarded.
#?(? (+) => 0 :after 1)
=> NIL

; In many cases, :around is useful instead of :before or :after.
; Inside of :aroud, you must call CALL-BODY like CL:CALL-NEXT-METHOD.
#?(? a => 0 :around (let((a 0))
		      (call-body)))
=> NIL

; :line is used to store file line internally.
#?(? t => NIL :line 123)
:satisfies (lambda($result)
	     (& (listp $result)
		(= 1 (length $result))
		(every #'issue-p $result)
		(= 123 (issue-position (car $result)))))

; :as is used internally to substitute.
; And it is specifyed via COMMON-REQUIREMENTS-ABOUT only.
#?(let((*org* (make-org)))
    (common-requirements-about (car first) :as command)
    ;; In order to delay macro expansion, EVAL is needed.
    ;; Because defspec refers *ORG* at macro expansion time.
    (eval '(defspec (command '(1 2 3)) => 1))
    (map-requirements #'check))
=> (NIL NIL)
,:test equal

; :timeout is used to set timeout.
; The default is 1 sec.
#?(? (sleep 0.1) => NIL :timeout 0.2)
=> NIL

; result := list which includes issues.

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about CHECK :doc-type function)

;;;; Description:
; accept requirement, then check it.
#?(check '((+) => 0))
=> NIL

#+syntax
(CHECK requirement) ; => result

;;;; Arguments and Values:

; requirement := unspecified.

; result := list which may includes issues.

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about MAKE-REQUIREMENT :doc-type function)

;;;; Description:
; make requirement.

#+syntax
(MAKE-REQUIREMENT &rest sb-pcl::args) ; => result

;;;; Argument Precedence Order:
; form key expected

;;;; Method signature:
; * (MAKE-REQUIREMENT (TEST-FORM T) (KEY (EQL :OUTPUT-SATISFIES)) (EXPECTED T) &REST PARAMETERS)
; * (MAKE-REQUIREMENT (TEST-FORM T) (KEY (EQL :EXPANDED-TO)) (EXPECTED T) &REST PARAMETERS)
; * (MAKE-REQUIREMENT (TEST-FORM T) (KEY (EQL :EQUIVALENTS)) (EXPECTED T) &REST PARAMETERS)
; * (MAKE-REQUIREMENT (TEST-FORM T) (KEY (EQL :BE-THE)) (EXPECTED T) &REST PARAMETERS)
; * (MAKE-REQUIREMENT (TEST-FORM T) (KEY (EQL :MULTIPLE-VALUE-SATISFIES)) (EXPECTED T) &REST PARAMETERS)
; * (MAKE-REQUIREMENT (TEST-FORM T) (KEY (EQL =>)) (EXPECTED (EQL IMPLEMENTATION-DEPENDENT)) &REST PARAMETERS)
; * (MAKE-REQUIREMENT (TEST-FORM T) (KEY (EQL =>)) (EXPECTED (EQL UNSPECIFIED)) &REST PARAMETERS)
; * (MAKE-REQUIREMENT (TEST-FORM T) (KEY (EQL :SATISFIES)) (EXPECTED T) &REST PARAMETERS)
; * (MAKE-REQUIREMENT (TEST-FORM T) (KEY (EQL :OUTPUTS)) (EXPECTED T) &REST PARAMETERS)
; * (MAKE-REQUIREMENT (TEST-FORM T) (KEY (EQL :VALUES)) (EXPECTED T) &REST PARAMETERS)
; * (MAKE-REQUIREMENT (TEST-FORM T) (KEY (EQL :INVOKES-DEBUGGER)) (EXPECTED T) &REST PARAMETERS)
; * (MAKE-REQUIREMENT (TEST-FORM T) (KEY (EQL :INVOKES-DEBUGGER)) (EXPECTED (EQL NOT)) &REST PARAMETERS)
; * (MAKE-REQUIREMENT (TEST-FORM T) (KEY (EQL :INVOKES-DEBUGGER)) (EXPECTED NULL) &REST PARAMETERS)
; * (MAKE-REQUIREMENT (TEST-FORM T) (KEY (EQL :SIGNALS)) (EXPECTED T) &REST PARAMETERS)

;;;; Arguments and Values:

; args :=

; result := lambda-form, but not specified.

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about THE-STANDARD-HANDLING-FORM :doc-type function)

;;;; Description:
; Helper for writing make-requirement.
#?(the-standard-handling-form 0 () 1 2 3)
=> (lambda()
     (let(0 (output ""))
       (handler-case(setf output (with-output-to-string(*standard-output*)
				   (with-integrated-output-stream(*standard-output*)
				     3)))
	 (warning(condition)
	   (push (make-instance 'warning-was-signaled :form '1 :expected '2
				:actual condition :position nil
				:message (princ-to-string condition))
		 0))
	 (error(condition)
	   (push (make-instance 'error-was-signaled :form '1 :expected '2
				:actual condition :position nil
				:message (princ-to-string condition))
		 0)))
       (unless(string= "" output)
	 (push (make-instance 'unexpected-output :form '1 :expected '""
			      :actual output :position nil)
	       0))
       0))
,:test sexp=

#+syntax
(THE-STANDARD-HANDLING-FORM result parameters test-form expected &rest body) ; => result

;;;; Arguments and Values:

; result := symbol as variable.

; parameters := plist which contains parameters.

; test-form := form which is tested.

; expected := expected value of test-form.

; body := form which make context which test is done.

; result := form

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about THE-PUSH-INSTANCE-FORM :doc-type function)

;;;; Description:
; helper for writing make-requirement.
#?(the-push-instance-form 0 1 2 3 4 5 6)
=> (push (make-instance '1 :form 2 :expected '3 :actual 4 :position 5 6)
	 0)
,:test equal

#+syntax
(THE-PUSH-INSTANCE-FORM place type test-form expected actual position &rest options) ; => result

;;;; Arguments and Values:

; place := symbol as variable.

; type := symbol as issue name.

; test-form := form which is tested.

; expected := value which is expected return value of test-form.

; actual := actual return value of test-form.

; position := non negative integer as file position.

; options := additional key value pair for issue constructor.

; result := form

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about RESERVED-KEYWORDS :doc-type function)

;;;; Description:
; return dispatch keys.
#?(reserved-keywords #'make-requirement)
:satisfies (lambda($result)
	     (& (listp $result)
		(null (set-difference $result
				      '(=> :be-the :satisfies :values :outputs :multiple-value-satisfies :output-satisfies :expanded-to :equivalents :signals :invokes-debugger)))))

#+syntax
(RESERVED-KEYWORDS gf) ; => result

;;;; Arguments and Values:

; gf := instance of make-requirement.

; result := list which contanis dispatch keys.

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about ENCALLABLE :doc-type function)

;;;; Description:
; tiny helper for writing macro.
#?(encallable ''car) => CAR
#?(encallable ''car t) => #'CAR
,:test equal
#?(encallable #'car) => CAR
#?(encallable #'car t) :be-the function
#?(encallable '#'car) => CAR
#?(encallable '(lambda(x)(print x))) => (LAMBDA (X) (PRINT X))
,:test equal
#?(encallable (lambda(x)(print x))) :be-the (cons (eql lambda)T)

#+syntax
(ENCALLABLE form &optional not-first-p) ; => result

;;;; Arguments and Values:

; form := function or function-name

; not-first-p := boolean

; result := function (when not-first-p is true)
; or function-name (when not-first-p is false (the default)).

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about CANONICALIZE :doc-type function)

;;;; Description:
; Helper for MAKE-REQUIREMENT
#?(canonicalize '(+) ())
:satisfies (lambda(form)
	     (equal form
		    (if bt:*supports-threads-p*
		      '(bt:with-timeout(1)
			 (+))
		      '(+))))

#+syntax
(CANONICALIZE test-form parameters) ; => result

;;;; Arguments and Values:

; test-form := form which is tested.

; parameters := key value pair.

; key := (member :before :after :around :lazy :timeout)

#?(canonicalize '(+) '(:before (print :before)))
:satisfies (lambda(form)
	     (equal form
		    `(progn (print :before)
			    ,(if bt:*supports-threads-p*
			       '(bt:with-timeout(1)
				  (+))
			       '(+)))))

#?(canonicalize '(+) '(:after (print :after)))
:satisfies (lambda(form)
	     (equal form
		    `(unwind-protect ,(if bt:*supports-threads-p*
					'(bt:with-timeout(1)
					   (+))
					'(+))
		       (print :after))))

#?(canonicalize '(+) '(:around (let((a 0))(call-body))))
:satisfies (lambda(form)
	     (equal form
		    `(let((a 0))
		       ,(if bt:*supports-threads-p*
			  '(bt:with-timeout(1)
			     (+))
			  '(+)))))

#?(canonicalize '(+) '(:lazy t))
:satisfies (lambda(form)
	     (equal form
		    (if bt:*supports-threads-p*
		      '(bt:with-timeout(1)
			 (eval (macroexpand '(+))))
		      '(eval (macroexpand '(+))))))

#?(canonicalize '(+) '(:timeout 2))
:satisfies (lambda(form)
	     (equal form
		    (if bt:*supports-threads-p*
		      '(bt:with-timeout(2)
			 (+))
		      '(+))))

; result := form.

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:
; form is copied.
#?(let*((form (list '+))
	(result(canonicalize form ())))
    (rplaca form '-)
    result)
:satisfies (lambda(form)
	     (equal form
		    (if bt:*supports-threads-p*
		      '(bt:with-timeout(1)(+))
		      '(+))))

;;;; Exceptional-Situations:
; when unsupported key comes (See ?), an error is signaled.
#?(canonicalize '(+) '(:no-such-key :comes)) :signals error

(common-requirements-about (issue test-issue wrong-format condition-issue error-was-signaled warning-was-signaled debugger-was-invoked unmatch-condition unexpected-success unexpected-output issue-of-multiple-values missing-restarts unsatisfied-clause)
			   :as object
			   :doc-type structure)

(defvar *test-issues* '(test-issue wrong-format))
(defvar *condition-issues* '(condition-issue error-was-signaled warning-was-signaled debugger-was-invoked unmatch-condition))
(defvar *issues* (cons 'issue (append *test-issues* *condition-issues*)))

;;;; [Structure] ISSUE
;;; inheritence DAG
; ```
; issue
;  |---test-issue
;  |    |---wrong-format
;  |---condition-issue
;  |    |---error-was-signaled
;  |    |---warning-was-signaled
;  |    |---debugger-was-invoked
;  |    |---unmatch-condition
;  |---unexpected-success
;  |---unexpected-output
;  |---issue-of-multiple-values
;  |---missing-restarts
;  |---unsatisfied-clause
; ```

;;;; Class Precedence List: (case in SBCL)
; issue structure-object slot-object t

;;;; Effective Slots:

; FORM [Type] T
#?(issue-form (make-instance 'object :form :get!)) => :get!

; EXPECTED [Type] T
#?(issue-expected (make-instance 'object :expected :get!)) => :get!

; ACTUAL [Type] T
#?(issue-actual (make-instance 'object :actual :get!)) => :get!

; POSITION [Type] T
#?(issue-position (make-instance 'object :position 0)) => 0

;;;; Notes:
; Can construct with MAKE-INSTANCE.
; Slot names are not exported.

(common-requirements-about (condition-issue error-was-signaled warning-was-signaled debugger-was-invoked unmatch-condition)
			   :as object
			   :doc-type structure)

;;;; Effective Slots:

; MESSAGE [Type] T
#?(condition-issue-message (make-instance 'object :message "message"))
=> "message"
,:test string=

;;;; Notes:

(common-requirements-about (test-issue wrong-format)
			   :as object
			   :doc-type structure)

;;;; Effective Slots:

; TEST [Type] T
#?(test-issue-test (make-instance 'object :test #'eql))
:satisfies (lambda($result)
	     (& (functionp $result)
		(eq 'eql (millet:function-name $result))))
;;;; Notes:

(requirements-about UNSATISFIED-CLAUSE :doc-type structure)

;;;; Effective Slots:

; ARGS [Type] T
#?(unsatisfied-clause-args (make-instance 'unsatisfied-clause :args '(:args :hoge)))
=> (:ARGS :HOGE)
,:test equal

;;;; Notes:

(requirements-about ISSUE-P :doc-type function)

;;;; Description:
; when arg is issue object, returns t, otherwise nil.
#?(let((issues (mapcar #'make-instance *issues*)))
    (every #'issue-p issues))
=> T

#+syntax
(ISSUE-P sb-kernel::object) ; => result

;;;; Arguments and Values:

; object := T
#?(issue-p 0) :invokes-debugger NOT

; result := boolean

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about CONDITION-ISSUE-P :doc-type function)

;;;; Description:
; Tests arg is condition-issue.
#?(let((issues(mapcar #'make-instance *condition-issues*)))
    (every #'condition-issue-p issues))
=> T
#?(let((issues(mapcar #'make-instance (set-difference *issues* *condition-issues*))))
    (notany #'condition-issue-p issues))
=> T

#+syntax
(CONDITION-ISSUE-P sb-kernel::object) ; => result

;;;; Arguments and Values:

; object := T
#?(condition-issue-p 0) :invokes-debugger not

; result := boolean

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about TEST-ISSUE-P :doc-type function)

;;;; Description:
; Tests arg is test-issue.
#?(let((issues(mapcar #'make-instance *test-issues*)))
    (every #'test-issue-p issues))
=> T
#?(let((issues(mapcar #'make-instance (set-difference *issues* *test-issues*))))
    (notany #'test-issue-p issues))
=> T

#+syntax
(TEST-ISSUE-P sb-kernel::object) ; => result

;;;; Arguments and Values:

; object := T
#?(test-issue-p :hoge) :invokes-debugger NOT

; result := boolean

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(common-requirements-about (error-was-signaled-p warning-was-signaled-p debugger-was-invoked-p unmatch-condition-p unexpected-success-p unexpected-output-p issue-of-multiple-values-p missing-restarts-p unsatisfied-clause-p wrong-format-p)
			   :as pred
			   :doc-type function)

;;;; [Predicates]

;;;; Description:
; Tests.
#?(let*((pred-name(symbol-name 'pred))
	(issue-name(find-symbol (subseq pred-name 0 (- (length pred-name) 2)))))
    (pred (make-instance issue-name)))
=> T
#+syntax
(ERROR-WAS-SIGNALED-P sb-kernel::object) ; => result

;;;; Arguments and Values:

; object := T
#?(pred 0) :invokes-debugger NOT

; result := boolean

;;;; Affected By:
; none

; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about ISSUE-FORM :doc-type function)

;;;; [Accessor] ISSUE-FORM

;;;; Description:
; return issue form.
#?(let((issues(loop :for name :in *issues*
		    :collect (make-instance name :form 0))))
    (loop :for i :in issues
	  :always (zerop (issue-form i))))
=> T

#+syntax
(ISSUE-FORM sb-kernel:instance) ; => result

#+setf
(SETF (ISSUE-FORM SB-KERNEL:INSTANCE) SB-KERNEL::VALUE) ; => new-value

;;;; Arguments and Values:

; instance := issue, otherwise error.
#?(issue-form 0) :signals ERROR
,:lazy T
,:ignore-signals warning

; result := form

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about ISSUE-EXPECTED :doc-type function)

;;;; Description: return expected value.
#?(loop :for name :in *issues*
	:always (zerop(issue-expected (make-instance name :expected 0))))
=> T

#+syntax
(ISSUE-EXPECTED sb-kernel:instance) ; => result

#+setf
(SETF (ISSUE-EXPECTED SB-KERNEL:INSTANCE) SB-KERNEL::VALUE) ; => new-value

;;;; Arguments and Values:

; instance := issue, otherwise error.
#?(issue-expected 0) :signals error
,:lazy t
,:ignore-signals warning

; result := expected value

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about ISSUE-ACTUAL :doc-type function)

;;;; Description:
; return actual value
#?(loop :for name :in *issues*
	:always (zerop (issue-actual (make-instance name :actual 0))))
=> T

#+syntax
(ISSUE-ACTUAL sb-kernel:instance) ; => result

#+setf
(SETF (ISSUE-ACTUAL SB-KERNEL:INSTANCE) SB-KERNEL::VALUE) ; => new-value

;;;; Arguments and Values:

; instance := issue, otherwise error.
#?(issue-actual 0) :signals error
,:lazy t
,:ignore-signals warning

; result := actual value

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about ISSUE-POSITION :doc-type function)

;;;; Description:
; return file position of test-form.
#?(loop :for name :in *issues*
	:always (zerop (issue-position(make-instance name :position 0))))
=> T

#+syntax
(ISSUE-POSITION sb-kernel:instance) ; => result

#+setf
(SETF (ISSUE-POSITION SB-KERNEL:INSTANCE) SB-KERNEL::VALUE) ; => new-value

;;;; Arguments and Values:

; instance := issue, otherwise error.
#?(issue-position 0) :signals error
,:lazy t
,:ignore-signals warning

; result := (or null non-negative-integer)

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about TEST-ISSUE-TEST :doc-type function)

;;;; Description:
; return test function name.
#?(loop :for name :in *test-issues*
	:always (eq 'equalp (test-issue-test(make-instance name :test 'equalp))))
=> T

#+syntax
(TEST-ISSUE-TEST sb-kernel:instance) ; => result

#+setf
(SETF (TEST-ISSUE-TEST SB-KERNEL:INSTANCE) SB-KERNEL::VALUE) ; => new-value

;;;; Arguments and Values:

; instance := test-issue, otherwise error.
#?(test-issue-test 0) :signals error
,:lazy t
,:ignore-signals warning
#?(loop :for name :in (set-difference *issues* *test-issues*)
	:never (ignore-errors(test-issue-test(make-instance name))))
=> T

; result := function name.

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about CONDITION-ISSUE-MESSAGE :doc-type function)

;;;; Description:
; return condition message string.
#?(loop :for name :in *condition-issues*
	:always (string= "message" (condition-issue-message (make-instance name :message "message"))))
=> T

#+syntax
(CONDITION-ISSUE-MESSAGE sb-kernel:instance) ; => result

#+setf
(SETF (CONDITION-ISSUE-MESSAGE SB-KERNEL:INSTANCE) SB-KERNEL::VALUE) ; => new-value

;;;; Arguments and Values:

; instance := condition-issue, otherwise error.
#?(loop :for name :in (set-difference *issues* *condition-issues*)
	:never (ignore-errors(condition-issue-message(make-instance name))))
=> T
#?(condition-issue-message 0) :signals error
,:lazy T
,:ignore-signals warning

; result := (or string null)

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about UNSATISFIED-CLAUSE-ARGS :doc-type function)

;;;; Description:
; return args.
#?(unsatisfied-clause-args(make-instance 'unsatisfied-clause :args :hoge))
=> :HOGE

#+syntax
(UNSATISFIED-CLAUSE-ARGS sb-kernel:instance) ; => result

#+setf
(SETF (UNSATISFIED-CLAUSE-ARGS SB-KERNEL:INSTANCE) SB-KERNEL::VALUE) ; => new-value

;;;; Arguments and Values:

; instance := unsatified-clause, otherwise error.
#?(loop :for name :in (remove 'unsatisfied-clause *issues*)
	:never (ignore-errors(unsatisfied-clause-args(make-instance name))))
=> T
#?(unsatisfied-clause-args 0) :signals error
,:lazy t
,:ignore-signals warning

; result := list which contains arg.

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about *PRINT-VIVID* :doc-type variable)

;;;; Description:
; Controls print vividly or not.

; Value type is BOOLEAN
#? *PRINT-VIVID* :be-the boolean

; Initial value is T

;;;; Affected By:
; jingoh.examiner:examine

;;;; Notes:
; refered by print-object specialized by issue, jingoh.tester::diff, and jingoh.tester::diff-string.

(requirements-about MISMATCH-SEXP :doc-type function)

;;;; Description:
; When sexp is not syntactically equal, markup such diffs.
#?(prin1 (mismatch-sexp :foo :bar))
:outputs #.(cl-ansi-text:red ":FOO")

#+syntax
(MISMATCH-SEXP actual expected) ; => result

;;;; Arguments and Values:

; actual := form

; expected := form

; result := form which may be markuped.
; *NOTE!* - markuped object is diff object. Do not confused.
#?(mismatch-sexp :foo :bar) :be-the jingoh.tester::diff

;;;; Affected By:
; `*print-vivid*` `jingoh.tester::*color-hook*`
; when `*print-vivid*` is nil, printed notation is not colored.
#?(let((*print-vivid* nil))
    (prin1 (mismatch-sexp :foo :bar)))
:outputs ":FOO"
#?(let((*print-vivid* nil))
    (princ (mismatch-sexp "foo" "foobar")))
:outputs "\"foo\""

;;;; Side-Effects:
; none

;;;; Notes:
; For debug use.

; Like SEXP=, this handle uninterned symbol in expected as variable. 
#?(mismatch-sexp '#:var 'hoge) => #:var
,:test (lambda($actual $result)
	 (& (symbolp $actual)
	    (symbolp $result)
	    (null (symbol-package $actual))
	    (null (symbol-package $result))
	    (string= (prin1-to-string $actual)
		     (prin1-to-string $result))))

;;;; Exceptional-Situations:

;;;; string-output tests
; When actual shorter than expected, string ":NULL" is added last.
#?(let((*color-hook* #'identity)) ; without coloring.
    (prin1 (mismatch-sexp "foo" "fooo")))
:outputs "\"foo:NULL\""

; when actual longer than expected, such part is printed with coloring.
#?(let((*color-hook* (constantly " instead of coloring")))
    (prin1 (mismatch-sexp "foobar" "foo")))
:outputs "\"foo instead of coloring\""

#?(let((*color-hook* (constantly "instead of coloring")))
    (prin1 (mismatch-sexp "foo" "bar")))
:outputs "\"instead of coloring\""

(requirements-about SYNTAX-ERROR :doc-type type)

;;;; Description:
; signaled when macro expansion time.

;;; Class Precedence List: (case in SBCL)
; syntax-error simple-error simple-condition program-error error serious-condition condition slot-object t

;;; Effective Slots:

; FORMAT-CONTROL [Type] T
; [READER] simple-condition-format-control

; FORMAT-ARGUMENTS [Type] T
; [READER] simple-condition-format-arguments

;;;; Notes:
