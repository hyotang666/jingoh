# [Macro] ?

## Syntax:

(? &body body) => result

## Arguments and Values:

body := arguments passed to MAKE-REQUIREMENT

result := list

## Description:
One shot checker.
Making requirement and CHECKing it.

## Example:

## Affected-By:

## Side-Effects:

## Notes:
When NIL is returned, it means requirement has no issued.

## Exceptional-Situations:
When syntax is not invalid, an error of type syntax-error is signaled.

## See-Also:

&
CHECK
IMPLEMENTATION-DEPENDENT
MAKE-REQUIREMENT
UNSPECIFIED
