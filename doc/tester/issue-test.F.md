# [Accessor] ISSUE-TEST

## Syntax:

(ISSUE-TEST arg) => result

(SETF (ISSUE-TEST ARG) new-value) => new-value

## Arguments and Values:

arg := issue object

result := (or null function-designator)

## Description:
Returns accepted issue's used predicate.
This may returns NIL, especially accepted issue is condition-issue because this is useless for such issues.

## Example:

## Affected-By:

## Side-Effects:

## Notes:

## Exceptional-Situations:
When arg is not issue, an error of type type-error is signaled.

## See-Also:

CONDITION-ISSUE
ERROR-WAS-SIGNALED
ISSUE
ISSUE-ACTUAL
ISSUE-EXPECTED
ISSUE-FORM
ISSUE-OF-MULTIPLE-VALUES
UNEXPECTED-SUCCESS
WARNING-WAS-SIGNALED
WRONG-FORMAT
