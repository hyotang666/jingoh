# [Accessor] CONDITION-ISSUE-MESSAGE

## Syntax:

(CONDITION-ISSUE-MESSAGE arg) => result

(SETF (CONDITION-ISSUE-MESSAGE ARG) new-value) => new-value

## Arguments and Values:

arg := condition-issue object

result := string

## Description:
Returns accepted condition-issue's condition's message string.

## Example:

## Affected-By:

## Side-Effects:

## Notes:

## Exceptional-Situations:
When arg is not condition-issue, an error of type type-error is signaled

## See-Also:

CONDITION-ISSUE
CONDITION-ISSUE-MESSAGE
ERROR-WAS-SIGNALED
WARNING-WAS-SIGNALED
