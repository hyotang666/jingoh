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
to check values, use :values keyword.
|#
#?(? (values 1 2) :values (1 2))
=> NIL

#|
to check output, use :output keyword.
|#
#?(? (print :foo) :output "
:FOO ")
=> NIL

#|
If behavior is not specified, you can use unspecified.
Samely, if return value is implementation dependent,
you can use implementation-dependent.
NOTE! - unspecified and implementation-dependent MUST be used with #. dispatch macro.
In case below, LISP-IMPLEMENTATION-TYPE is implementation dependent.
NOTE! - unspecified and implementation-dependent is just for checking signals or not.
|#
#?(? (lisp-implementation-type) => #.implementation-dependent)
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
#1?(? 0 :no-such-keyword :hoge)
:signals syntax-error
:lazy t

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
:output "0 1"

(requirements-about &)

#|
Like CL:AND, this is asserts all form is returns non nil value.
|#
#1?(macroexpand-1 '(& (symbolp 'foo)))
=> (OR (ASSERT (SYMBOLP 'FOO))
       T)
:test equal

(requirements-about internal-dsl)

#|
Supported keywords are returned by RESERVED-KEYWORDS.
|#
#?(reserved-keywords #'make-requirement)
:satisfies #`(null(set-exclusive-or $result
				    '(=> :signals :output :satisfies :values :multiple-value-satisfies :invoke-debugger-with :never-invoke-debugger)))

#|
encallable makes argument to fits lisp forms first element.
|#
#?(encallable 'car) => CAR
#?(encallable #'car) => CAR
#1?(encallable '(lambda()(print :foo))) => (LAMBDA()(PRINT :FOO))
:test equal

#|
if optional argument is passed as true, applyable form is returned.
|#
#1?(encallable '#'car t) => #'CAR
:test equal
