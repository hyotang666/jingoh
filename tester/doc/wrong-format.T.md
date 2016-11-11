# [Structure] WRONG-FORMAT

## Class Precedence List: (case in CLISP)

* wrong-format issue structure-object t

## Effective Slots:

* FORM [Type] T
[ACCESSOR] issue-form
Contains test form which output string to stream.

* EXPECTED [Type] T
[ACCESSOR] issue-expected
Contains FORM's expected generated string.

* ACTUAL [Type] T
[ACCESSOR] issue-actual
Contains FORM's actual generated string.

* TEST [Type] T
[ACCESSOR] issue-test
Contains predicates which tests EXPECTED and ACTUAL.
The default is CL:STRING=.

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
ISSUE-POSITION
ISSUE-TEST
MISSING-RESTARTS
TEST-ISSUE
UNEXPECTED-OUTPUT
UNEXPECTED-SUCCESS
UNMATCH-CONDITION
UNSATISFIED-CLAUSE
WARNING-WAS-SIGNALED
WRONG-FORMAT
WRONG-FORMAT-P

