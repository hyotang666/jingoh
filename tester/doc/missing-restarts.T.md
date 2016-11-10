# [Structure] MISSING-RESTARTS

## Class Precedence List: (case in CLISP)

* missing-restarts issue structure-object t

## Effective Slots:

* FORM [Type] T
[ACCESSOR] issue-form
Contains test form which tested.

* EXPECTED [Type] T
[ACCESSOR] issue-expected
Contains specified restart name or restart names list.

* ACTUAL [Type] T
[ACCESSOR] issue-actual
Contains the restart object which actualy invoke debugger.

* TEST [Type] T
[ACCESSOR] issue-test
NIL because nonsence.

## Description:
MISSING-RESTARTS is the issue about condition was signaled but specified restarts is not found.

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
ISSUE-POSITION
ISSUE-TEST
MISSING-RESTARTS
UNEXPECTED-OUTPUT
UNEXPECTED-SUCCESS
UNMATCH-CONDITION
WARNING-WAS-SIGNALED
WRONG-FORMAT

