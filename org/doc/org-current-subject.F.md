# [Accessor] ORG-CURRENT-SUBJECT

## Syntax:

(ORG-CURRENT-SUBJECT org) => result

(SETF (ORG-CURRENT-SUBJECT org) new-value) => new-value

## Arguments and Values:

org := org

new-value := subject-designator

result := subject-designator

## Description:
Accepts org object, returns its current subject.

## Example:

## Affected-By:

## Side-Effects:

## Notes:

## Exceptional-Situations:
When org is not org, an error of type type-error is signaled.

## See-Also:

`*SUBJECT*`
