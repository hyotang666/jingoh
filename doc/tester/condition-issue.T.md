# [Structure] CONDITION-ISSUE

## Class Precedence List: (case in CLISP)

* condition-issue issue structure-object t

## Effective Slots:

* MESSAGE [Type] T
[ACCESSOR] condition-issue-message
Contains condition message string.

* FORM [Type] T
[ACCESSOR] issue-form
Contains lisp form which causes signaling condition.

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
Condition issue is superclass for issue about conditions.
When you extends new issue about conditions, it must inherit this.

## Example:

## Notes: 
Usually this is not used directly, just for inheritance.

## See Also:

CONDITION-ISSUE-MESSAGE
CONDITION-ISSUE-P
DEBUGGER-WAS-INVOKED
ERROR-WAS-SIGNALED
ISSUE
ISSUE-ACTUAL
ISSUE-EXPECTED
ISSUE-FORM
ISSUE-POSITION
ISSUE-TEST
WARNING-WAS-SIGNALED

