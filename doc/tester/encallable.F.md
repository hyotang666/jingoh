# [Function] ENCALLABLE

## Syntax:

(ENCALLABLE form &optional not-first-p) => result

## Arguments and Values:

form := function designator form

not-first-p := boolean

result := callable form

## Description:
Helper for writing macro to avoid annoying funcall.
But when not-first-p is true, the form which for fits to be argument is returned.

## Example:
```lisp
(encallable ''car) => CAR
(encallable ''car t) => #'CAR
(encallable #'car) => CAR
(encallable #'car t) => #'CAR
(encallable '#'car) => CAR
(encallable '(lambda(x)(print x))) => (LAMBDA (X) (PRINT X))
(encallable (lambda(x)(print x))) => (LAMBDA(&REST ARGS)
                                       (APPLY #<FUNCTION LAMBDA> ARGS))
```

## Affected-By:

## Side-Effects:

## Notes:

## Exceptional-Situations:
when form is not function designator, an error of type syntax-error is signaled.

## See-Also:

CANONICALIZE
MAKE-REQUIREMENT
SYNTAX-ERROR
THE-PUSH-INSTANCE-FORM
THE-STANDARD-HANDLING-FORM
