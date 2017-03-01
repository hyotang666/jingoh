(defpackage :jingoh.tester.spec
  (:use :cl :jingoh :jingoh.tester :jingoh.org))
(in-package :jingoh.tester.spec)
(setup :jingoh.tester.spec)

;;;; main apis for light users.

(requirements-about DEFSPEC :around (let((*org* (make-org)))
				      (call-body)))

;;;; [Macro] DEFSPEC

#| Description: Define specification. |#

#+syntax
(DEFSPEC &body body) ; => result

;;; Arguments and Values:

#| body := see ? below. |#

#| result := current subjects. |#
#?(defspec (+) => 0) => (NIL)
,:test equal

#| Affected By: *org* |#

#| Side-Effects: Modify *ORG*'s specification slot. |#
#?(progn (princ (org-requirements-count *org*))
	 (defspec (+) => 0)
	 (princ (org-requirements-count *org*)))
:outputs "01"

#| Notes: |#

#| Exceptional-Situations: |#

(requirements-about &)

;;;; [Macro] &

#| Description: When (cl:and ...) is failed, we are not able to get information
   about which clause is failed.
   & provides such information. |#
#?(& T T T) => T
#?(& T NIL T) :signals jingoh.tester::UNSATISFIED

#+syntax
(& &body body) ; => result

;;; Arguments and Values:

#| body := some forms which generates non NIL value. |#

#| result := When all form return non NIL value, returns T.
             Otherwise error. |#

#| Affected By: none |#

#| Side-Effects: none |#

#| Notes: |#
;; When one form is evaluated to NIL, short cut is occur.
#?(& T NIL (princ :hoge)) :signals jingoh.tester::UNSATISFIED
;; When cluase is variable, macro form, or special form, 
;; :args slot is stored with NIL.
#?(handler-case(& (eq :hoge :fuga))
    (jingoh.tester::unsatisfied(c)
      (jingoh.tester::args c)))
=> (:hoge :fuga)
,:test equal
#?(handler-case(& (and (eq :hoge :fuga)))
    (jingoh.tester::unsatisfied(c)
      (jingoh.tester::args c)))
=> NIL

#| Exceptional-Situations: |#

(requirements-about SEXP=)

;;;; [Function] SEXP=

#| Description: tests equalilty as syntax. 
	        This is useful to test MACRO-EXPENDed form. |#
#?(sexp= '(let((#0=#:var 0)) #0#) '(let((var 0))var))
=> T
#?(sexp= '#:foo 'var) => T
#?(sexp= 'foo '#:foo) => NIL

#+syntax
(SEXP= sexp1 sexp2) ; => result

;;; Arguments and Values:

#| sexp1 := form, which may include GENSYMed symbol. |#

#| sexp2 := form. |#

#| result := boolean |#

#| Affected By: none |#

#| Side-Effects: none |#

#| Notes: Still alpha quality. |#

#| Exceptional-Situations: |#

(requirements-about ?)

;;;; [Macro] ?

#| Description: One shot tester. |#
#?(? (+) => 0) => NIL
#?(? (+) => 1) :satisfies #`(& (listp $result)
			       (= 1 (length $result))
			       (every #'issue-p $result))

#+syntax
(? &body body) ; => result

;;; Arguments and Values:

#| body := (test-form dispatch-key expected option*)
   test-form := form which is tested.
   dispatch-key := (member => :be-the :satisfies :outputs :values :multiple-value-satisfies :output-satisfies :expanded-to :equivalents :signals :invokes-debugger)
   expected := lisp value which is expected value of test-form.
   option := key value pair.
   key := (member :test :lazy :ignore-signals :with-restarts :stream :before :after :around :position :as)
|#

;;; dispatch-keys.

;; standard usage.
#?(? (+) => 0) => NIL
;; When expected value is unreadable object, use :be-the.
#?(? #'car :be-the function) => NIL
;; Above code can write like below.
#?(? #'car :satisfies functionp) => NIL
;; To test multiple values, use :values
#?(? (floor 2 3) :values (0 2)) => NIL
;; To test output strings, use :outputs
#?(? (princ :foo) :outputs "FOO") => NIL
;; To test complex multiple values, use :multiple-value-satisfies.
#?(? (values 0 *standard-output*)
     :multiple-value-satisfies (lambda(n s)
				 (& (numberp n)
				    (streamp s))))
=> NIL
;; To test complex string, use :output-satisfies
; TODO
;; To test condition, use :signals.
#?(? (error "error") :signals ERROR) => NIL
;; To test debugger is invoked or not, use :invokes-debugger
#?(? (invoke-debugger(make-condition 'error)) :invokes-debugger ERROR)
=> NIL
;; To test macro expansion, use :expanded-to
#?(& T) :expanded-to (progn (assert t ()'jingoh.tester::unsatisfied
				    :test-form 'T)
			    T)
;; To test two codes are semantically equal, use :equivalents
#?(loop :for i :in '(1 2 3) :sum i)
:equivalents (reduce #'+ '(1 2 3))

;;; option keys

;; When expected value is not symbol, character or integer, use :test option.
#?(? (list 1 2 3) => (1 2 3) :test equal)
=> NIL
;; To ignore condition especially warning, use :ignore-signals.
#?(? (progn (warn "warning")
	    t)
     => T :ignore-signals warning)
=> NIL
;; To test compile time error is occur, use :lazy.
#?(? (defun "invalid"()(princ :hoge))
     :signals ERROR :lazy T)
=> NIL
;; To test restart is available, use :with-restarts with :signals.
#?(? (warn "warning") :signals warning :with-restarts muffle-warning)
=> NIL
;; When dispatch key is :outputs, you can specify stream by :stream.
#?(? (princ :foo *error-output*) :outputs "FOO" :stream *error-output*)
=> NIL
;; When test needs setup, you can use :before.
#?(? (princ :foo) :outputs "HOGEFOO" :before (princ :hoge))
=> NIL
;; When test needs teardown, you can use :after.
#?(? (princ :foo) :outputs "FOOBAR" :after (princ :bar))
=> NIL
;; NOTE! Internally, :after is placed as clean up form of CL:UNWIND-PROTECT,
;;	 so :after's return value is discarded.
#?(? (+) => 0 :after 1)
=> NIL
;; In many cases, :around is useful instead of :before or :after.
;; Inside of :aroud, you must call CALL-BODY like CL:CALL-NEXT-METHOD.
#?(? a => 0 :around (let((a 0))
		      (call-body)))
=> NIL
;; :position is used to store file-position internally.
#?(? t => NIL :position 123)
:satisfies #`(& (listp $result)
		(= 1 (length $result))
		(every #'issue-p $result)
		(= 123 (issue-position (car $result))))
;; :as is used internally to substitute.
;; And it is specifyed via COMMON-REQUIREMENTS-ABOUT only.
#?(let((*org* (make-org)))
    (common-requirements-about (car first) :as command)
    ;; In order to delay macro expansion, EVAL is needed.
    ;; Because defspec refer *ORG* at macro expansion time.
    (eval '(defspec (command '(1 2 3)) => 1))
    (map-requirements #'check))
=> (NIL NIL)
,:test equal

#| result := list which includes issues. |#

#| Affected By: none |#

#| Side-Effects: none |#

#| Notes: |#

#| Exceptional-Situations: |#

;;;; internal api for hackers.

(requirements-about CHECK)

;;;; [Function] CHECK

#| Description: accept requirement, then check it. |#
#?(check '((+) => 0))
=> NIL

#+syntax
(CHECK requirement) ; => result

;;; Arguments and Values:

#| requirement := unspecified. |#

#| result := list which may includes issues.|#

#| Affected By: none |#

#| Side-Effects: none |#

#| Notes: |#

#| Exceptional-Situations: |#

(requirements-about MAKE-REQUIREMENT)

;;;; [Generic-Function] MAKE-REQUIREMENT

#| Description: make requirement. |#

#+syntax
(MAKE-REQUIREMENT &rest sb-pcl::args) ; => result

#| Argument Precedence Order:
form key expected
|#

#| Method signature:
(MAKE-REQUIREMENT (TEST-FORM T) (KEY (EQL :OUTPUT-SATISFIES)) (EXPECTED T)
                  &REST PARAMETERS)
(MAKE-REQUIREMENT (TEST-FORM T) (KEY (EQL :EXPANDED-TO)) (EXPECTED T) &REST
                  PARAMETERS)
(MAKE-REQUIREMENT (TEST-FORM T) (KEY (EQL :EQUIVALENTS)) (EXPECTED T) &REST
                  PARAMETERS)
(MAKE-REQUIREMENT (TEST-FORM T) (KEY (EQL :BE-THE)) (EXPECTED T) &REST
                  PARAMETERS)
(MAKE-REQUIREMENT (TEST-FORM T) (KEY (EQL :MULTIPLE-VALUE-SATISFIES))
                  (EXPECTED T) &REST PARAMETERS)
(MAKE-REQUIREMENT (TEST-FORM T) (KEY (EQL =>))
                  (EXPECTED (EQL IMPLEMENTATION-DEPENDENT)) &REST PARAMETERS)
(MAKE-REQUIREMENT (TEST-FORM T) (KEY (EQL =>)) (EXPECTED (EQL UNSPECIFIED))
                  &REST PARAMETERS)
(MAKE-REQUIREMENT (TEST-FORM T) (KEY (EQL :SATISFIES)) (EXPECTED T) &REST
                  PARAMETERS)
(MAKE-REQUIREMENT (TEST-FORM T) (KEY (EQL :OUTPUTS)) (EXPECTED T) &REST
                  PARAMETERS)
(MAKE-REQUIREMENT (TEST-FORM T) (KEY (EQL :VALUES)) (EXPECTED T) &REST
                  PARAMETERS)
(MAKE-REQUIREMENT (TEST-FORM T) (KEY (EQL :INVOKES-DEBUGGER)) (EXPECTED T)
                  &REST PARAMETERS)
(MAKE-REQUIREMENT (TEST-FORM T) (KEY (EQL :INVOKES-DEBUGGER))
                  (EXPECTED (EQL NOT)) &REST PARAMETERS)
(MAKE-REQUIREMENT (TEST-FORM T) (KEY (EQL :INVOKES-DEBUGGER)) (EXPECTED NULL)
                  &REST PARAMETERS)
(MAKE-REQUIREMENT (TEST-FORM T) (KEY (EQL :SIGNALS)) (EXPECTED T) &REST
                  PARAMETERS)
(MAKE-REQUIREMENT (TEST-FORM T) (KEY (EQL =>)) (EXPECTED T) &REST PARAMETERS)
|#

;;; Arguments and Values:

#| args := |#

#| result := lambda-form, but not specified. |#

#| Affected By: none |#

#| Side-Effects: none |#

#| Notes: |#

#| Exceptional-Situations: |#

(requirements-about THE-STANDARD-HANDLING-FORM)

;;;; [Function] THE-STANDARD-HANDLING-FORM

#| Description: Helper for writing make-requirement. |#
#?(the-standard-handling-form 0 () 1 2 3)
=> (lambda()
     (let(0 (output ""))
       (handler-case(setf output (with-output-to-string(*terminal-io*)
				   3))
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

;;; Arguments and Values:

#| result := symbol as variable.|#

#| parameters := plist which contains parameters. |#

#| test-form := form which is tested. |#

#| expected := expected value of test-form. |#

#| body := form which make context which test is done. |#

#| result := form |#

#| Affected By: none |#

#| Side-Effects: none |#

#| Notes: |#

#| Exceptional-Situations: |#

(requirements-about THE-PUSH-INSTANCE-FORM)

;;;; [Function] THE-PUSH-INSTANCE-FORM

#| Description: helper for writing make-requirement. |#
#?(the-push-instance-form 0 1 2 3 4 5 6)
=> (push (make-instance '1 :form 2 :expected '3 :actual 4 :position 5 6)
	 0)
,:test equal

#+syntax
(THE-PUSH-INSTANCE-FORM place type test-form expected actual position &rest options) ; => result

;;; Arguments and Values:

#| place := symbol as variable. |#

#| type := symbol as issue name. |#

#| test-form := form which is tested. |#

#| expected := value which is expected return value of test-form. |#

#| actual := actual return value of test-form. |#

#| position := non negative integer as file position. |#

#| options := additional key value pair for issue constructor. |#

#| result := form |#

#| Affected By: none |#

#| Side-Effects: none |#

#| Notes: |#

#| Exceptional-Situations: |#

(requirements-about RESERVED-KEYWORDS)

;;;; [Function] RESERVED-KEYWORDS

#| Description: return dispatch keys. |#
#?(reserved-keywords #'make-requirement)
:satisfies #`(& (listp $result)
		(null (set-difference $result
				      '(=> :be-the :satisfies :values :outputs :multiple-value-satisfies :output-satisfies :expanded-to :equivalents :signals :invokes-debugger))))

#+syntax
(RESERVED-KEYWORDS gf) ; => result

;;; Arguments and Values:

#| gf := instance of make-requirement. |#

#| result := list which contanis dispatch keys. |#

#| Affected By: none |#

#| Side-Effects: none |#

#| Notes: |#

#| Exceptional-Situations: |#

(requirements-about ENCALLABLE)

;;;; [Function] ENCALLABLE

#| Description: tiny helper for writing macro. |#
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

;;; Arguments and Values:

#| form := function or function-name |#

#| not-first-p := boolean |#

#| result := function (when not-first-p is true)
	     or function-name (when not-first-p is false (the default)).
|#

#| Affected By: none |#

#| Side-Effects: none |#

#| Notes: |#

#| Exceptional-Situations: |#

(requirements-about CANONICALIZE)

;;;; [Function] CANONICALIZE

#| Description: Helper for MAKE-REQUIREMENT |#
#?(canonicalize '(+) ()) => (+)
,:test equal

#+syntax
(CANONICALIZE test-form parameters) ; => result

;;; Arguments and Values:

#| test-form := form which is tested. |#

#| parameters := key value pair.
   key := (member :before :after :around :lazy)|#
#?(canonicalize '(+) '(:before (print :before)))
=> (progn (print :before)
	  (+))
,:test equal
#?(canonicalize '(+) '(:after (print :after)))
=> (unwind-protect (+)
     (print :after))
,:test equal
#?(canonicalize '(+) '(:around (let((a 0))(call-body))))
=> (let((a 0))(+))
,:test equal
#?(canonicalize '(+) '(:lazy t))
=> (eval '(+))
,:test equal

#| result := form.|#

#| Affected By: none |#

#| Side-Effects: none |#

#| Notes: form is copied. |#
#?(let((form '(+)))
    (eq form (canonicalize form ())))
=> NIL

#| Exceptional-Situations: when unsupported key comes (See ?), an error is signaled. |#
#?(canonicalize '(+) '(:no-such-key :comes)) :signals error

(common-requirements-about (issue test-issue wrong-format condition-issue error-was-signaled warning-was-signaled debugger-was-invoked unmatch-condition unexpected-success unexpected-output issue-of-multiple-values missing-restarts unsatisfied-clause)
			   :as object)

(defvar *test-issues* '(test-issue wrong-format))
(defvar *condition-issues* '(condition-issue error-was-signaled warning-was-signaled debugger-was-invoked unmatch-condition))
(defvar *issues* (cons 'issue (append *test-issues* *condition-issues*)))
#|[Structure] ISSUE
;;;; inheritence DAG
issue
 |---test-issue
 |    |---wrong-format
 |---condition-issue
 |    |---error-was-signaled
 |    |---warning-was-signaled
 |    |---debugger-was-invoked
 |    |---unmatch-condition
 |---unexpected-success
 |---unexpected-output
 |---issue-of-multiple-values
 |---missing-restarts
 |---unsatisfied-clause
|#

;; Class Precedence List: (case in SBCL)
;; issue structure-object slot-object t

;; Effective Slots:

;; FORM [Type] T
#?(issue-form (make-instance 'object :form :get!)) => :get!

;; EXPECTED [Type] T
#?(issue-expected (make-instance 'object :expected :get!)) => :get!

;; ACTUAL [Type] T
#?(issue-actual (make-instance 'object :actual :get!)) => :get!

;; POSITION [Type] T
#?(issue-position (make-instance 'object :position 0)) => 0

#| Notes: Can construct with MAKE-INSTANCE.
	  Slot names are not exported. |#

(common-requirements-about (condition-issue error-was-signaled warning-was-signaled debugger-was-invoked unmatch-condition)
			   :as object)

;; Effective Slots:

;; MESSAGE [Type] T
#?(condition-issue-message (make-instance 'object :message "message"))
=> "message"
,:test string=

#| Notes: |#

(common-requirements-about (test-issue wrong-format)
			   :as object)

;; Effective Slots:

;; TEST [Type] T
#?(test-issue-test (make-instance 'object :test #'eql))
:satisfies #`(& (functionp $result)
		(eq 'eql (millet:function-name $result)))
#| Notes: |#

(requirements-about UNSATISFIED-CLAUSE)

;; Effective Slots:

;; ARGS [Type] T
#?(unsatisfied-clause-args (make-instance 'unsatisfied-clause :args '(:args :hoge)))
=> (:ARGS :HOGE)
,:test equal

#| Notes: |#

(requirements-about ISSUE-P)

;;;; [Function] ISSUE-P

#| Description: when arg is issue object, returns t, otherwise nil. |#
#?(let((issues (mapcar #'make-instance *issues*)))
    (every #'issue-p issues))
=> T

#+syntax
(ISSUE-P sb-kernel::object) ; => result

;;; Arguments and Values:

#| object := T |#
#?(issue-p 0) :invokes-debugger NOT

#| result := boolean |#

#| Affected By: none |#

#| Side-Effects: none |#

#| Notes: |#

#| Exceptional-Situations: |#

(requirements-about CONDITION-ISSUE-P)

;;;; [Function] CONDITION-ISSUE-P
#?(let((issues(mapcar #'make-instance *condition-issues*)))
    (every #'condition-issue-p issues))
=> T
#?(let((issues(mapcar #'make-instance (set-difference *issues* *condition-issues*))))
    (notany #'condition-issue-p issues))
=> T

#| Description: Tests arg is condition-issue. |#

#+syntax
(CONDITION-ISSUE-P sb-kernel::object) ; => result

;;; Arguments and Values:

#| object := T |#
#?(condition-issue-p 0) :invokes-debugger not

#| result := boolean |#

#| Affected By: none |#

#| Side-Effects: none |#

#| Notes: |#

#| Exceptional-Situations: |#

(requirements-about TEST-ISSUE-P)

;;;; [Function] TEST-ISSUE-P

#| Description: Tests arg is test-issue. |#
#?(let((issues(mapcar #'make-instance *test-issues*)))
    (every #'test-issue-p issues))
=> T
#?(let((issues(mapcar #'make-instance (set-difference *issues* *test-issues*))))
    (notany #'test-issue-p issues))
=> T

#+syntax
(TEST-ISSUE-P sb-kernel::object) ; => result

;;; Arguments and Values:

#| object := T |#
#?(test-issue-p :hoge) :invokes-debugger NOT

#| result := boolean |#

#| Affected By: none |#

#| Side-Effects: none |#

#| Notes: |#

#| Exceptional-Situations: |#

(common-requirements-about (error-was-signaled-p warning-was-signaled-p debugger-was-invoked-p unmatch-condition-p unexpected-success-p unexpected-output-p issue-of-multiple-values-p missing-restarts-p unsatisfied-clause-p wrong-format-p)
			   :as pred)

;;;; [Predicates]

#| Description: Tests. |#
#?(let*((pred-name(symbol-name 'pred))
	(issue-name(find-symbol (subseq pred-name 0 (- (length pred-name) 2)))))
    (pred (make-instance issue-name)))
=> T
#+syntax
(ERROR-WAS-SIGNALED-P sb-kernel::object) ; => result

;;; Arguments and Values:

#| object := T |#
#?(pred 0) :invokes-debugger NOT

#| result := boolean |#

#| Affected By: none |#

#| Side-Effects: none |#

#| Notes: |#

#| Exceptional-Situations: |#

(requirements-about ISSUE-FORM)

;;;; [Accessor] ISSUE-FORM

#| Description: return issue form. |#
#?(let((issues(loop :for name :in *issues*
		    :collect (make-instance name :form 0))))
    (loop :for i :in issues
	  :always (zerop (issue-form i))))
=> T

#+syntax
(ISSUE-FORM sb-kernel:instance) ; => result

#+setf
(SETF (ISSUE-FORM SB-KERNEL:INSTANCE) SB-KERNEL::VALUE) ; => new-value

;;; Arguments and Values:

#| instance := issue, otherwise error. |#
#?(issue-form 0) :signals ERROR
,:lazy T
,:ignore-signals warning

#| result := form |#

#| Affected By: none |#

#| Side-Effects: none |#

#| Notes: |#

#| Exceptional-Situations: |#

(requirements-about ISSUE-EXPECTED)

;;;; [Accessor] ISSUE-EXPECTED

#| Description: return expected value. |#
#?(loop :for name :in *issues*
	:always (zerop(issue-expected (make-instance name :expected 0))))
=> T

#+syntax
(ISSUE-EXPECTED sb-kernel:instance) ; => result

#+setf
(SETF (ISSUE-EXPECTED SB-KERNEL:INSTANCE) SB-KERNEL::VALUE) ; => new-value

;;; Arguments and Values:

#| instance := issue, otherwise error. |#
#?(issue-expected 0) :signals error
,:lazy t
,:ignore-signals warning

#| result := expected value |#

#| Affected By: none |#

#| Side-Effects: none |#

#| Notes: |#

#| Exceptional-Situations: |#

(requirements-about ISSUE-ACTUAL)

;;;; [Accessor] ISSUE-ACTUAL

#| Description: return actual value |#
#?(loop :for name :in *issues*
	:always (zerop (issue-actual (make-instance name :actual 0))))
=> T

#+syntax
(ISSUE-ACTUAL sb-kernel:instance) ; => result

#+setf
(SETF (ISSUE-ACTUAL SB-KERNEL:INSTANCE) SB-KERNEL::VALUE) ; => new-value

;;; Arguments and Values:

#| instance := issue, otherwise error. |#
#?(issue-actual 0) :signals error
,:lazy t
,:ignore-signals warning

#| result := actual value |#

#| Affected By: none |#

#| Side-Effects: none |#

#| Notes: |#

#| Exceptional-Situations: |#

(requirements-about ISSUE-POSITION)

;;;; [Accessor] ISSUE-POSITION

#| Description: return file position of test-form. |#
#?(loop :for name :in *issues*
	:always (zerop (issue-position(make-instance name :position 0))))
=> T

#+syntax
(ISSUE-POSITION sb-kernel:instance) ; => result

#+setf
(SETF (ISSUE-POSITION SB-KERNEL:INSTANCE) SB-KERNEL::VALUE) ; => new-value

;;; Arguments and Values:

#| instance := issue, otherwise error. |#
#?(issue-position 0) :signals error
,:lazy t
,:ignore-signals warning

#| result := (or null non-negative-integer) |#

#| Affected By: none |#

#| Side-Effects: none |#

#| Notes: |#

#| Exceptional-Situations: |#

(requirements-about TEST-ISSUE-TEST)

;;;; [Accessor] TEST-ISSUE-TEST

#| Description: return test function name. |#
#?(loop :for name :in *test-issues*
	:always (eq 'equalp (test-issue-test(make-instance name :test 'equalp))))
=> T

#+syntax
(TEST-ISSUE-TEST sb-kernel:instance) ; => result

#+setf
(SETF (TEST-ISSUE-TEST SB-KERNEL:INSTANCE) SB-KERNEL::VALUE) ; => new-value

;;; Arguments and Values:

#| instance := test-issue, otherwise error. |#
#?(test-issue-test 0) :signals error
,:lazy t
,:ignore-signals warning
#?(loop :for name :in (set-difference *issues* *test-issues*)
	:never (ignore-errors(test-issue-test(make-instance name))))
=> T

#| result := function name. |#

#| Affected By: none |#

#| Side-Effects: none |#

#| Notes: |#

#| Exceptional-Situations: |#

(requirements-about CONDITION-ISSUE-MESSAGE)

;;;; [Accessor] CONDITION-ISSUE-MESSAGE

#| Description: return condition message string. |#
#?(loop :for name :in *condition-issues*
	:always (string= "message" (condition-issue-message (make-instance name :message "message"))))
=> T

#+syntax
(CONDITION-ISSUE-MESSAGE sb-kernel:instance) ; => result

#+setf
(SETF (CONDITION-ISSUE-MESSAGE SB-KERNEL:INSTANCE) SB-KERNEL::VALUE) ; => new-value

;;; Arguments and Values:

#| instance := condition-issue, otherwise error. |#
#?(loop :for name :in (set-difference *issues* *condition-issues*)
	:never (ignore-errors(condition-issue-message(make-instance name))))
=> T
#?(condition-issue-message 0) :signals error
,:lazy T
,:ignore-signals warning

#| result := (or string null) |#

#| Affected By: none |#

#| Side-Effects: none |#

#| Notes: |#

#| Exceptional-Situations: |#

(requirements-about UNSATISFIED-CLAUSE-ARGS)

;;;; [Accessor] UNSATISFIED-CLAUSE-ARGS

#| Description: return args. |#
#?(unsatisfied-clause-args(make-instance 'unsatisfied-clause :args :hoge))
=> :HOGE

#+syntax
(UNSATISFIED-CLAUSE-ARGS sb-kernel:instance) ; => result

#+setf
(SETF (UNSATISFIED-CLAUSE-ARGS SB-KERNEL:INSTANCE) SB-KERNEL::VALUE) ; => new-value

;;; Arguments and Values:

#| instance := unsatified-clause, otherwise error. |#
#?(loop :for name :in (remove 'unsatisfied-clause *issues*)
	:never (ignore-errors(unsatisfied-clause-args(make-instance name))))
=> T
#?(unsatisfied-clause-args 0) :signals error
,:lazy t
,:ignore-signals warning

#| result := list which contains arg. |#

#| Affected By: none |#

#| Side-Effects: none |#

#| Notes: |#

#| Exceptional-Situations: |#

(requirements-about *PRINT-VIVID*)

#| [Variable] *PRINT-VIVID* Controls print vividly or not. |#

;; Value type is BOOLEAN
#? *PRINT-VIVID* :be-the boolean

;; Initial value is T

#| Affected By: none |#

#| Notes: refered by print-object specialized by issue, jingoh.tester::diff, and jingoh.tester::diff-string. |#

(requirements-about SYNTAX-ERROR)

#|[Condition] SYNTAX-ERROR signaled when macro expansion time. |#

;; Class Precedence List: (case in SBCL)
;; syntax-error simple-error simple-condition program-error error serious-condition condition slot-object t

;; Effective Slots:

;; FORMAT-CONTROL [Type] T
;; [READER] simple-condition-format-control

;; FORMAT-ARGUMENTS [Type] T
;; [READER] simple-condition-format-arguments

#| Notes: |#

