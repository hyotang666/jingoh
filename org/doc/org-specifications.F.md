# [Accessor] ORG-SPECIFICATIONS

## Syntax:

(ORG-SPECIFICATIONS org) => result

(SETF (ORG-SPECIFICATIONS org) new-value) => new-value

## Arguments and Values:

org := org

result := unspecified

## Description:
Accepts org, returns specifications object.

## Example:

## Affected-By:

## Side-Effects:

## Notes:
For extensibility, specifications's concrete data structure is not specified.
Although jingoh.org provides enough api to manupilate it.
Users (include hackers) does not need to know what is happen behind the black box.

## Exceptional-Situations:
When org is not org object, an error of type type-error is signaled.

## See-Also:

`*SUBJECT*`
ADD-REQUIREMENT
DELETE-SUBJECT
DO-REQUIREMENTS
MAP-REQUIREMENTS
ORG
ORG-CURRENT-SUBJECT
ORG-REQUIREMENTS-COUNT
REQUIREMENTS-ABOUT
