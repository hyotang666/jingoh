# [Function] CANONICALIZE

## Syntax:

(CANONICALIZE test-form parameters) => result

## Arguments and Values:

test-form := one lisp form

parameters := parameter-plist usually it is MAKE-REQUIREMENT parameter named params

result := lisp form

## Description:
When parameters have key :lazy, and its value is true, returns test-form witch wrapped with CL:EVAL.
This works as (eval-when(:execute)...).

When parameters have key :lazy, and its value is NIL, evaluate test-form then return test-form.
This works as (eval-when(:compile-toplevel)...).

When parameters have key :around, returns test-form witch wrapped with around form.
(This is like CLOS CALL-NEXT-METHOD.)

This is helper for MAKE-REQUIREMENT

## Example:
```lisp
(canonicalize :test '())
=> :TEST
(canonicalize :test '(:lazy t))
=> (EVAL ':TEST)
(canonicalize :test '(:around (let((a 1))(call-body))))
=> (LET ((A 1)) :TEST)
```

## Affected-By:

## Side-Effects:
When :lazy is specified NIL explicitly, test-form is evaluated.

## Notes:
This is intended to be used to control compiling.
Usually jingoh compiles test forms.
It means macro is expanded.
For instance, when you want to test invalid macro form signals condition, you need to control compiling because such macro should be expanded in run time.

## Exceptional-Situations:

## See-Also:

ENCALLABLE
MAKE-REQUIREMENT
THE-PUSH-INSTANCE-FORM
THE-STANDARD-HANDLING-FORM
