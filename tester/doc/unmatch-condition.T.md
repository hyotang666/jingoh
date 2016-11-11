# [Structure] UNMATCH-CONDITION

## Class Precedence List: (case in CLISP)

* unmatch-condition condition-issue issue structure-object t

## Effective Slots:

* FORM [Type] T
[ACCESSOR] issue-form
Contains test form which invoked debugger but with unexpected condition.

* EXPECTED [Type] T
[ACCESSOR] issue-expected
Contains condition type which should invoke debugger.

* ACTUAL [Type] T
[ACCESSOR] issue-actual
Contains condition type which actualy invoked debugger.

## Description:
UNMATCH-CONDITION is the issue that debugger was invoked but argument was not match expected type.

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
ISSUE-POSITION
MISSING-RESTARTS
TEST-ISSUE
UNEXPECTED-OUTPUT
UNEXPECTED-SUCCESS
UNMATCH-CONDITION-P
UNSATISFIED-CLAUSE
WARNING-WAS-SIGNALED
WRONG-FORMAT

