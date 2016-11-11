# [Accessor] ISSUE-ACTUAL

## Syntax:

(ISSUE-ACTUAL arg) => result

(SETF (ISSUE-ACTUAL ARG) new-value) => new-value

## Arguments and Values:

arg := issue object

result := any lisp object

## Description:
Returns accepted issue's actual result.

## Example:

## Affected-By:

## Side-Effects:

## Notes:

## Exceptional-Situations:
When arg is not issue object, an error of type type-error is signaled.

## See-Also:

CONDITION-ISSUE
DEBUGGER-WAS-INVOKED
ERROR-WAS-SIGNALED
ISSUE
ISSUE-EXPECTED
ISSUE-FORM
ISSUE-OF-MULTIPLE-VALUES
ISSUE-POSITION
MISSING-RESTARTS
TEST-ISSUE
UNEXPECTED-OUTPUT
UNEXPECTED-SUCCESS
UNMATCH-CONDITION
UNSATISFIED-CLAUSE
WARNING-WAS-SIGNALED
WRONG-FORMAT
