# [Structure] UNEXPECTED-SUCCESS

## Class Precedence List: (case in CLISP)

* unexpected-success issue structure-object t

## Effective Slots:

* FORM [Type] T
[ACCESSOR] issue-form
Contains lisp form which did not signal condition.

* EXPECTED [Type] T
[ACCESSOR] issue-expected
Contains FORM's expected signaled condition name.

* ACTUAL [Type] T
[ACCESSOR] issue-actual
Contains FORM's actual return value.

* TEST [Type] T
[ACCESSOR] issue-test
NIL because useless.

## Description:
UNEXPECTED-SUCCESS is the issue which expected condition was not signaled.

## Example:

## Notes:

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
ISSUE-TEST
WARNING-WAS-SIGNALED
WRONG-FORMAT

