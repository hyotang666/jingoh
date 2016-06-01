# [Function] RESERVED-KEYWORDS

## Syntax:

(RESERVED-KEYWORDS gf) => result

## Arguments and Values:

gf := MAKE-REQUIREMENT generic-function

result := list

## Description:
Return the list which consist of currently supported keywords.

## Example:

## Affected-By:

## Side-Effects:

## Notes:
This is called by no-applicable-method in order to generate more helpful error message.

## Exceptional-Situations:

## See-Also:

?
DEFSPEC
MAKE-REQUIREMENT
