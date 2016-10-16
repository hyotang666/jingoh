# [Function] MAKE-ORG

## Syntax:

(MAKE-ORG &key 
          (name nil)
	  (current-subject nil)
	  (specifications (make-array 0 :fill-pointer 0 :adjustable t)))
=> result

## Arguments and Values:

name := symbol

current-subject := subject-designator

specifications := unspecified

result := org

## Description:
Creates new org object, return it.

## Example:

## Affected-By:

## Side-Effects:

## Notes:
For extensibility, specification's concrete data type is not specified.
Users should not use this directly.
Using provided abstract operators instead is strongly recommended.

## Exceptional-Situations:
When argument does not satisfy type declaration, an error of type type-error may signaled (implementation dependent).

## See-Also:

DEFORG
ORG
ORG-DESIGNATOR
REGISTER-ORG
