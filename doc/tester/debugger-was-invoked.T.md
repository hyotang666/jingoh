# [Structure] DEBUGGER-WAS-INVOKED

## Class Precedence List: (case in CLISP)

* debugger-was-invoked condition-issue issue structure-object t

## Effective Slots:

* MESSAGE [Type] T
[ACCESSOR] condition-issue-message
Contains condition message string.

* FORM [Type] T
[ACCESSOR] issue-form
Contains lisp form which invokes debugger.

* EXPECTED [Type] T
[ACCESSOR] issue-expected
NIL bacause useless.

* ACTUAL [Type] T
[ACCESSOR] issue-actual
Contains signaled condition.

* TEST [Type] T
[ACCESSOR] issue-test
NIL bacause useless.

## Description:
DEBUGGER-WAS-INVOKED is the issue of unexpected invoking debugger.

## Example:

## Notes:

## See Also:

CONDITION-ISSUE
CONDITION-ISSUE-MESSAGE
CONDITION-ISSUE-P
DEBUGGER-WAS-INVOKED-P
ERROR-WAS-SIGNALED-P
ISSUE
ISSUE-ACTUAL
ISSUE-EXPECTED
ISSUE-FORM
ISSUE-OF-MULTIPLE-VALUES
ISSUE-P
ISSUE-TEST
UNEXPECTED-SUCCESS
WARNING-WAS-SIGNALED
WRONG-FORMAT
