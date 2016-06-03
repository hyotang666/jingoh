# [Structure] WRONG-FORMAT

## Class Precedence List: (case in CLISP)

* wrong-format issue structure-object t

## Effective Slots:

* FORM [Type] T
[ACCESSOR] issue-form
Contains lisp form which output string to stream.

* EXPECTED [Type] T
[ACCESSOR] issue-expected
Contains FORM's expected generated string.

* ACTUAL [Type] T
[ACCESSOR] issue-actual
Contains FORM's actual generated string.

* TEST [Type] T
[ACCESSOR] issue-test
Contains predicates which tests EXPECTED and ACTUAL.
NOTE! - Test may fail even if EXPECTED and ACTUAL is the same value.
e.g. (eq '(A) '(A)) => NIL

## Description:
WRONG-FORMAT is the issue about outputted string.

## Example:

## Notes:
Currently byte stream output is not supported.

## See Also:

CONDITION-ISSUE
DEBUGGER-WAS-INVOKED
ERROR-WAS-SIGNALED
ISSUE
ISSUE-ACTUAL
ISSUE-EXPECTED
ISSUE-FORM
ISSUE-OF-MULTIPLE-VALUES
ISSUE-P
ISSUE-POSITION
ISSUE-TEST
UNEXPECTED-SUCCESS
WARNING-WAS-SIGNALED
WRONG-FORMAT
WRONG-FORMAT-P

