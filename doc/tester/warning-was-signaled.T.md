# [Structure] WARNING-WAS-SIGNALED

## Class Precedence List: (case in CLISP)

* warning-was-signaled condition-issue issue structure-object t

## Effective Slots:

* MESSAGE [Type] T
[ACCESSOR] condition-issue-message
Contains condition message string.

* FORM [Type] T
[ACCESSOR] issue-form
Contains lisp form which causes signaling warning.

* EXPECTED [Type] T
[ACCESSOR] issue-expected
Contains FORM's expected return value.

* ACTUAL [Type] T
[ACCESSOR] issue-actual
Contains signaled condition.

* TEST [Type] T
[ACCESSOR] issue-test
NIL bacause useless.

## Description:
WARNING-WAS-SIGNALED is the issue of unexpected warning.

## Example:

## Notes:

## See Also:

CONDITION-ISSUE
CONDITION-ISSUE-MESSAGE
CONDITION-ISSUE-P
DEBUGGER-WAS-INVOKED
ERROR-WAS-SIGNALED
ISSUE
ISSUE-ACTUAL
ISSUE-EXPECTED
ISSUE-FORM
ISSUE-OF-MULTIPLE-VALUES
ISSUE-P
ISSUE-TEST
UNEXPECTED-SUCCESS
WARNING-WAS-SIGNALED-P
WRONG-FORMAT
