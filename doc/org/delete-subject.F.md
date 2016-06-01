# [Function] DELETE-SUBJECT

## Syntax:

(DELETE-SUBJECT subject &optional (org `*org*`)) => result

## Arguments and Values:

subject := subject-designator

org := org object

result := T

## Description:
Delete specified subject from specified org.
When org is not specified, the default is current org (i.e. `*org*`).
When subject is T, current subject (i.e. `*subject*`) is deleted.
When subject is NIL, all subjects are deleted.
Otherwise specified one is deleted.

## Example:

## Affected-By:

## Side-Effects:
Specified org specification's status may modified.

## Notes:
Unlike CL:REMHASH, DELETE-SUBJECT does not sign entry exists or not.
Always return T, even if specified subject does not exist in specified org.

Even if delete current subject, the value of `*subject*` is not change.

## Exceptional-Situations:
When specified org is not org object, an error of type not-org is signaled.

## See-Also:

`*ORG*`
`*SUBJECT*`
ORG
ORG-CURRENT-SUBJECT
ORG-SPECIFICATIONS
SUBJECT-DESIGNATOR
