(in-package :cl-user)
(defpackage :design-?(:use :cl :jingoh.tester :jingoh.reader :jingoh.org :jingoh.reporter)
  (:import-from :jingoh.tester #:sexp=))
(in-package :design-?)

(jingoh:setup :jingoh.tester)

;;;;
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
        use :invokes-debugger keyword.
|#
#?(? (signal 'error) :invokes-debugger not)
=> #.unspecified
;; ecl invokes debugger, but others
;; ANSI-CL say 'If the condition is not handled, signal returns nil.'
;; but 'toplevel never handle the condition.'
;; i.e. it is unpsecified.
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
to check values, use :values keyword.
|#
#?(? (values 1 2) :values (1 2))
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
to check return type, you can use :be-the keyword.
|#
#?(? (make-pathname) :be-the pathname)
=> NIL

#|
to check with complex test, you can use :satisfies keyword.
|#
#?(? #P"" :satisfies pathnamep)
=> NIL

#|
satisfies accepts lambda form.
|#
#?(? (make-pathname) :satisfies (lambda(result)
				  (& (pathnamep result)
				     (= 0 (length(namestring result))))))
=> NIL

#|
to check complex output format, you can use :output-satisfies keyword.
|#
#?(? (format t "foo :bar (bazz)")
     :output-satisfies (lambda(formatted-string)
			 (with-input-from-string(s formatted-string)
			   (& (eq 'foo (read s))
			      (eq :bar (read s))
			      (equal '(bazz) (read s))))))
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
#?(let((*org*(make-org :current-subjects '(:foo))))
    (defspec (+ 1 1) => 2))
=> (:FOO)
,:test equal

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
#?(& (symbolp 'foo))
:expanded-to
(PROGN
  (ASSERT(SYMBOLP 'FOO)()
    'JINGOH.TESTER::UNSATISFIED :TEST-FORM '(SYMBOLP 'FOO) :ARGS (LIST 'FOO))
  T)

;;;;
(requirements-about internal-dsl)

#|
Supported keywords are returned by RESERVED-KEYWORDS.
|#
#?(reserved-keywords #'make-requirement)
:satisfies #`(null(set-exclusive-or $result
				    '(=> :signals :outputs :satisfies :values :multiple-value-satisfies :invokes-debugger :be-the :equivalents :expanded-to :output-satisfies)))

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

#|
sexp= is tests sexp1 and sexp2 is same sexp.
Its behavior is almost same with CL:EQUAL, but can handle uninterned symbol.
This is designed for test macroexpanded form.
|#
#?(sexp= (let((var(gensym "VAR")))
	   `(let((,var 0)),var))
	 '(let((foo 0)) foo))
=> T

#|
NOTE! - Macroexpanded form must be first argument.
|#
#?(sexp= '(let((foo 0)) foo)
	 '(let((#0=#:foo 0)) #0#))
=> NIL

#|
NOTE! - For its raison d'etre, uninterned symbol is treat as special.
If arg1 is uninterned symbol and arg2 is symbol, keeps its pair.
And evaluated to be T.
|#
#?(sexp= '#:foo 'bar) => T
#?(sexp= '#:foo 0) => NIL
#?(sexp= '(#0=#:foo #0#) '(hoge hoge)) => T
#?(sexp= '(#0=#:foo #0#) '(hoge fuga)) => NIL

#|
In common lisp, vector can be written as literal.
Macro may generate vector, which may contains GENSYMed symbol.
SEXP= can handle it.
|#
#?(sexp= #(#:foo #:bar) #(foo bar)) => T

#|
Also array.
|#
#?(sexp= #2A((1 #:foo)) #2A((1 foo))) => T
#|
Also structure.
|#
(eval-when(:compile-toplevel :load-toplevel)
  (defstruct foo bar)
  (defmethod make-load-form((s foo)&optional environment)
    (make-load-form-saving-slots s :environment environment))
  )
#?(sexp= #S(foo :bar #:foo) #S(foo :bar foo)) => T
