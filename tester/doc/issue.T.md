# [Structure] ISSUE

## Class Precedence List: (case in CLISP)
```
wrong-format ---------- test-issue --------------+- issue - structure-object - T
		        issue-of-multiple-values |
		        unexpected-success ------|
		        unexpected-output -------|
		        missing-restarts --------|
		        unsatisfied-clause ------|
error-was-signaled --+- condition-issue ---------+
warning-was-signaled |
debugger-was-invoked |
unmatch-condition ---+
```

## Effective Slots:
### of issue
* FORM [Type] T
[ACCESSOR] issue-form
Contains test form which causes issue.

* EXPECTED [Type] T
[ACCESSOR] issue-expected
Contains expected return value of FORM.

* ACTUAL [Type] T
[ACCESSOR] issue-actual
Contains actual return value of FORM.

* POSITION [Type] T
[ACCESSOR] issue-position
Contains file position if any.

### of test-issue
* TEST [Type] T
[ACCESSOR] test-issue-test
Contains test function which accepts EXPECTED and ACTUAL.

### of condtiion-issue
* MESSAGE [Type] T
[ACCESSOR] condition-issue-message
Contains condition printed message.

### of unsatisfied-clause
* ARGS [Type] T
[ACCESSOR] unsatisfied-clause-args
May contains list which each element is actual arguments.

## Description:
ISSUE is abstract data type which manages requirement's issue, and this is the superclass (prisicely it is structure though.) of all issues.
Issues are just an internal data structure, and used as protocol.
So like CL:CONDITION system, light users may not touch this directly, but hackers.

Issues are produced by tester, and used by reporter in order to show the details of requirement's issue.

## Example:

## Notes:

## See Also:

CONDITION-ISSUE
CONDITION-ISSUE-MESSAGE
CONDITION-ISSUE-P
DEBUGGER-WAS-INVOKED
DEBUGGER-WAS-INVOKED-P
ERROR-WAS-SIGNALED
ERROR-WAS-SIGNALED-P
ISSUE
ISSUE-ACTUAL
ISSUE-EXPECTED
ISSUE-FORM
ISSUE-OF-MULTIPLE-VALUES
ISSUE-OF-MULTIPLE-VALUES-P
ISSUE-P
ISSUE-POSITION
ISSUE-TEST
MISSING-RESTARTS
MISSING-RESTARTS-P
TEST-ISSUE
TEST-ISSUE-P
TEST-ISSUE-TEST
UNEXPECTED-OUTPUT
UNEXPECTED-OUTPUT-P
UNEXPECTED-SUCCESS
UNEXPECTED-SUCCESS-P
UNMATCH-CONDITION
UNMATCH-CONDITION-P
UNSATISFIED-CLAUSE
UNSATISFIED-CLAUSE-ARGS
UNSATISFIED-CLAUSE-P
WARNING-WAS-SIGNALED
WARNING-WAS-SIGNALED-P
WRONG-FORMAT
WRONG-FORMAT-P

