# [Structure] ISSUE

## Class Precedence List: (case in CLISP)

* issue structure-object t

## Effective Slots:

* FORM [Type] T
[ACCESSOR] issue-form
Contains test form which causes issue.

* EXPECTED [Type] T
[ACCESSOR] issue-expected
Contains FORM's expected return value.

* ACTUAL [Type] T
[ACCESSOR] issue-actual
Contains FORM's actual return value.

* TEST [Type] T
[ACCESSOR] issue-test
Contains predicates which tests EXPECTED and ACTUAL.
NOTE! - Test may fail even if EXPECTED and ACTUAL is the same value.
e.g. (eq '(A) '(A)) => NIL

## Description:
ISSUE is abstract data type which manages requirement's issue, and this is the superclass of all issues.
Issues are just an internal data structure, and used as protocol.
So like CL:CONDITION system, light users may not touch this directly, but hackers.

Issues are produced by tester, and used by reporter in order to show the details of requirement's issue.

## Example:

## See Also:

CONDITION-ISSUE
DEBUGGER-WAS-INVOKED
ERROR-WAS-SIGNALED
ISSUE-ACTUAL
ISSUE-EXPECTED
ISSUE-FORM
ISSUE-OF-MULTIPLE-VALUES
ISSUE-P
ISSUE-POSITION
ISSUE-TEST
MAKE-REQUIREMENT
MISSING-RESTARTS
UNEXPECTED-OUTPUT
UNEXPECTED-SUCCESS
UNMATCH-CONDITION
WARNING-WAS-SIGNALED
WRONG-FORMAT

## Notes:
When you extends make-requirement, and if you need to add new issue structure, it must inherit (:include) one of issue object.

