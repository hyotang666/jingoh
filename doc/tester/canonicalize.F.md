# [Function] CANONICALIZE

## Syntax:

(CANONICALIZE test-form parameters) => result

## Arguments and Values:

test-form := one lisp form

parameters := parameter-plist usually it is MAKE-REQUIREMENT parameter named params

result := lisp form

## Description:
When parameters have key :lazy, and its value is true, returns test-form witch wrapped with CL:EVAL.
This is helper for MAKE-REQUIREMENT

## Example:
```lisp
(canonicalize :test '())
=> :TEST
(canonicalize :test '(:lazy t))
=> (EVAL ':TEST)
```

## Affected-By:

## Side-Effects:

## Notes:
This is intended to be used to avoid compiling.

## Exceptional-Situations:

## See-Also:

ENCALLABLE
MAKE-REQUIREMENT
THE-PUSH-INSTANCE-FORM
THE-STANDARD-HANDLING-FORM
