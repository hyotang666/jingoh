# [Accessor] ISSUE-FORM

## Syntax:

(ISSUE-FORM arg) => result

(SETF (ISSUE-FORM ARG) new-value) => new-value

## Arguments and Values:

arg := issue object

result := one lisp form

## Description:
Returns accepted issue's caused test form.

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
ISSUE-ACTUAL
ISSUE-EXPECTED
ISSUE-OF-MULTIPLE-VALUES
ISSUE-POSITION
ISSUE-TEST
MISSING-RESTARTS
UNEXPECTED-OUTPUT
UNEXPECTED-SUCCESS
WARNING-WAS-SIGNALED
WRONG-FORMAT
