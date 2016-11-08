# [Function] THE-STANDARD-HANDLING-FORM

## Syntax:

(THE-STANDARD-HANDLING-FORM result parameters test-form expected &rest body) => result

## Arguments and Values:

result := symbol as variable.

parameters := plist.

test-form := symbol as variable which contains test-form.

expected := symbol as variable which contains expected.

body := any lisp forms.

result := form

## Description:
Template for common signal handling form.

## Example:
```lisp
(the-standard-handling-form 0 nil 1 2 3 4 5)
=>
(LAMBDA ()
  (PROG (0 #:OUTPUT111)
    (HANDLER-BIND ((WARNING (LAMBDA (CONDITION)
                              (DECLARE (IGNORABLE CONDITION))
                              (UNLESS NIL
			        (PUSH (MAKE-INSTANCE 'WARNING-WAS-SIGNALED
				                     :FORM '1
						     :EXPECTED '2
						     :ACTUAL CONDITION
						     :POSITION NIL
						     :MESSAGE (PRINC-TO-STRING CONDITION))
                                      0))
                              (GO :END)))
                   (ERROR (LAMBDA (CONDITION)
		            (PUSH (MAKE-INSTANCE 'ERROR-WAS-SIGNALED
			                         :FORM '1
						 :EXPECTED '2
						 :ACTUAL CONDITION
						 :POSITION NIL
						 :MESSAGE (PRINC-TO-STRING CONDITION))
				  0)
			    (GO :END))))
      (UNWIND-PROTECT (SETF #:OUTPUT111
                            (WITH-OUTPUT-TO-STRING (*TERMINAL-IO*)
			      (PROGN 3 4 5))) ; <- as body.
        NIL))
    (UNLESS (STRING= "" #:OUTPUT111)
      (PUSH (MAKE-INSTANCE 'UNEXPECTED-OUTPUT
                           :FORM 1
			   :EXPECTED '""
			   :ACTUAL #:OUTPUT111
			   :POSITION NIL)
            0))
    :END
    (RETURN 0)))
```

## Affected-By:

## Side-Effects:

## Notes:
This is the helper for MAKE-REQUIREMENT.

## Exceptional-Situations:
PARAMETERS is used, when it is not plist, an error is signaled.

## See-Also:

CANONICALIZE
ENCALLABLE
RESERVED-KEYWORDS
SYNTAX-ERROR
THE-PUSH-INSTANCE-FORM
THE-STANDARD-HANDLING-FORM
