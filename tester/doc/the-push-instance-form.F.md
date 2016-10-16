# [Function] THE-PUSH-INSTANCE-FORM

## Syntax:

(THE-PUSH-INSTANCE-FORM place type test-form expected actual position &rest options) => result

## Arguments and Values:

place := symbol as variable.

type := symbol as issue name.

test-form := symbol as variable which contains test-form.

expected := symbol as variable which contains expected.

actual := symbol as variable which contains actual.

position := integer as file-position

params := key value pair for constructing TYPE.

result := form

## Description:
Template for constructor and pushing operation.

## Example:
```lisp
(the-push-instance-form 0 1 2 3 4 5 6)
=>
(PUSH (MAKE-INSTANCE '1 :FORM '2 :EXPECTED '3 :ACTUAL 4 :POSITION 5 6) 0)
```

## Affected-By:

## Side-Effects:

## Notes:
This is the helper for THE-STANDARD-HANDLING-FORM and MAKE-REQUIREMENT.

## Exceptional-Situations:

## See-Also:

MAKE-REQUIREMENT
THE-STANDARD-HANDLING-FORM
