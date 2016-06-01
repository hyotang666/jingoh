# [Function] MAP-REQUIREMENTS

## Syntax:

(MAP-REQUIREMENTS function &optional (subject T) (org `*org*`))
=> result

## Arguments and Values:

function := function-designator which takes one argument

subject := subject-designator

org := org object

result := list

## Description:
Like CL:MAPCAR, MAP-REQUIREMENTS do the mapping operation involves applying FUNCTION to successive sets of an argument in which is obtained from SUBJECT's requirements.

When subject is T, current subject (i.e. `*subject*`) 's requirements are iterated (the default).
When subject is NIL, all requirements in specifications are iterated.
Otherwise specified subject's requirements are iterated.

FUNCTION is called each requirement.
If function is a symbol, it is coerced to a function as if by CL:SYMBOL-FUNCTION.
The iteration terminates when exhausted every requirements.
The value returned by MAP-REQUIREMENTS is a list of the results of successive calls to function.

An implicit block named NIL surrounds MAP-REQUIREMENTS.
CL:RETURN may be used to terminate the loop immediately without performing any further iterations, returning zero or more values.

## Example:

## Affected-By:

## Side-Effects:

## Notes:

## Exceptional-Situations:
When org is not org object, an error of type not-org is signaled.
When specified subject is not found, an error of type missing-subject is signaled.

## See-Also:

`*ORG*`
`*SUBJECT*`
DO-REQUIREMENTS
MISSING-SUBJECT
NOT-ORG
ORG
ORG-CURRENT-SUBJECT
ORG-SPECIFICATIONS
SUBJECT-DESIGNATOR
