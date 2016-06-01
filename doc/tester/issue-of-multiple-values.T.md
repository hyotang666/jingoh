# [Structure] ISSUE-OF-MULTIPLE-VALUES

## Class Precedence List: (case in CLISP)

* issue-of-multiple-values issue structure-object t

## Effective Slots:

* FORM [Type] T
[ACCESSOR] issue-form
Contains lisp form which produce multiple values.

* EXPECTED [Type] T
[ACCESSOR] issue-expected
Contains the list which consists of FORM's expected return values.

* ACTUAL [Type] T
[ACCESSOR] issue-actual
Contains the list which consists of actual FORM's return values.

* TEST [Type] T
[ACCESSOR] issue-test
Contains predicates which tests EXPECTED and ACTUAL.
NOTE! - Test may fail even if EXPECTED and ACTUAL is the same value.
e.g. (eq '(A) '(A)) => NIL

## Description:
ISSUE-OF-MULTIPLE-VALUES is the issue especially about multiple values.

## Example:

## Notes:

## See Also:

CONDITION-ISSUE
ERROR-WAS-SIGNALED
ISSUE
ISSUE-ACTUAL
ISSUE-EXPECTED
ISSUE-FORM
ISSUE-OF-MULTIPLE-VALUES
ISSUE-TEST
UNEXPECTED-SUCCESS
WARNING-WAS-SIGNALED
WRONG-FORMAT

