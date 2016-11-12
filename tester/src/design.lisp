(in-package :cl-user)
(defpackage :design-?(:use :cl :jingoh.tester :jingoh.reader :jingoh.org :jingoh.reporter))
(in-package :design-?)

(jingoh:setup :jingoh.tester)

(requirements-about ?)

#|
? is one shot requirement tester.
When requirement is true, returns nil.
|#
#?(? (+) => 0) => NIL

#|
When requirement is false, returns issue list.
|#
#?(? (+) => 1) :satisfies #`(& (listp $result)
			       (= 1 (length $result))
			       (every #'issue-p $result))

#|
to check signals or not, use :signals keyword.
|#
#?(? (error "test") :signals error)
=> NIL
#|
NOTE! - :signals keyword check just signaled only.
        If you want to check actualy debugger is invoked or not,
        use :invoke-debugger keyword.
|#
#?(? (signal 'error) :invokes-debugger NIL)
=> NIL
, :ignore-signals T
#?(? (signal 'error) :invokes-debugger error)
:satisfies consp
, :ignore-signals T
#?(? (warn 'program-error) :invokes-debugger program-error)
:satisfies consp
, :ignore-signals T
#?(? (warn 'warning) :invokes-debugger warning)
:satisfies consp
, :ignore-signals T
#?(?( warn 'error) :invokes-debugger TYPE-ERROR)
=> NIL
, :ignore-signals T
#?(? (error 'warning) :invokes-debugger WARNING)
=> NIL
, :ignore-signals T

#|
to check values, use values specifier.
(Remember VALUES type specifier.)
|#
#?(? (values 1 2) => (values 1 2))
=> NIL

#|
to check output, use :output keyword.
|#
#?(? (print :foo) :outputs "
:FOO ")
=> NIL

#|
If return value is implementation dependent,
you can use implementation-dependent.
In such case, just side effect (include signals) only checked.
NOTE! - implementation-dependent MUST be used with #. dispatch macro.
Example below, LISP-IMPLEMENTATION-TYPE is implementation dependent.
|#
#?(? (lisp-implementation-type) => #.implementation-dependent)
=> NIL

#|
If behavior is not specified,
you can use unspecified.
In such case, nothing checked.
Always success.
NOTE! - unspecified MUST be used with #. dispatch macro.
|#
#?(? (error "test") => #.unspecified)
=> NIL

#|
to check with complex test, you can use :satisfies keyword.
|#
#?(? #P"" :satisfies pathnamep)
=> NIL

#|
satisfies accepts lambda form.
|#
#?(? #P"" :satisfies (lambda(result)
		       (& (pathnamep result)
			  (= 0 (length(namestring result))))))
=> NIL

#|
Unknown keyword comes, an error will be signaled.
|#
#?(? 0 :no-such-keyword :hoge)
:signals syntax-error
, :lazy t

(requirements-about defspec)

#|
DEFSPEC defines new specifications of requirement.
Returns currnt *subject*
|#
#?(let((*org*(make-org :current-subject :foo)))
    (defspec (+ 1 1) => 2))
=> :FOO

#|
Current org (i.e. *org*) is modified.
|#
#?(let((*org*(make-org)))
    (princ(org-requirements-count *org*)) ; initially no requirements.
    (defspec (+ 1 1) => 2) ; side effect!
    (write-char #\space)
    (princ (org-requirements-count *org*))) ; now modified.
:outputs "0 1"

(requirements-about &)

#|
Like CL:AND, this is asserts all form is returns non nil value.
|#
#?(macroexpand-1 '(& (symbolp 'foo)))
=> (OR (UNLESS (SYMBOLP 'FOO)
	  (ERROR 'JINGOH.TESTER::UNSATISFIED :TEST-FORM '(SYMBOLP 'FOO) :ARGS (LIST 'FOO)))
       T)
, :test equal

(requirements-about internal-dsl)

#|
Supported keywords are returned by RESERVED-KEYWORDS.
|#
#?(reserved-keywords #'make-requirement)
:satisfies #`(null(set-exclusive-or $result
				    '(=> :signals :outputs :satisfies :values :multiple-value-satisfies :invokes-debugger)))

#|
encallable makes argument to fits lisp forms first element.
|#
#?(encallable 'car) => CAR
#?(encallable #'car) => CAR
#?(encallable '(lambda()(print :foo))) => (LAMBDA()(PRINT :FOO))
, :test equal

#|
if optional argument is passed as true, applyable form is returned.
|#
#?(encallable '#'car t) => #'CAR
, :test equal
