# [Structure] UNEXPECTED-OUTPUT

## Class Precedence List: (case in CLISP)

* unexpected-output issue structure-object t

## Effective Slots:

* FORM [Type] T
[ACCESSOR] issue-form
Contains test form which output unexpectedly.

* EXPECTED [Type] T
[ACCESSOR] issue-expected
Empty string. (i.e. "")

* ACTUAL [Type] T
[ACCESSOR] issue-actual
Outputted string.

* TEST [Type] T
[ACCESSOR] issue-test
NIL because useless.

## Description:
UNEXPECTED-OUTPUT is the issue which expected no output occur.

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
UNEXPECTED-SUCCESS
UNMATCH-CONDITION
WARNING-WAS-SIGNALED
WRONG-FORMAT
