# [Structure] TEST-ISSUE

## Class Precedence List: (case in CLISP)

* test-issue issue structure-object t

## Effective Slots:

* TEST [Type] T
[ACCESSOR] test-issue-test
Contains test function designator.

* FORM [Type] T
[ACCESSOR] issue-form
Contains test form which causes issue.

* EXPECTED [Type] T
[ACCESSOR] issue-expected
Contains FORM's expected return value.

* ACTUAL [Type] T
[ACCESSOR] issue-actual
Contains FORM's actual return value.

* POSITION [Type] T
[ACCESSOR] issue-position
Contains file position if any.

## Description:

## Example:

## Notes:

## See Also:

ISSUE
ISSUE-ACTUAL
ISSUE-EXPECTED
ISSUE-FORM
ISSUE-OF-MULTIPLE-VALUES
ISSUE-P
ISSUE-POSITION
TEST-ISSUE
TEST-ISSUE-P
TEST-ISSUE-TEST
WRONG-FORMAT

