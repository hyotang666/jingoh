# [Function] DELETE-ORG

## Syntax:

(DELETE-ORG org-designator) => result

## Arguments and Values:

org-designator := org-designator

result := T

## Description:
Deletes specified org from underlying org database.

## Example:

## Affected-By:
Existing orgs.

## Side-Effects:
Underlying org database is modified.

## Notes:

## Exceptional-Situations:
When specified org does not exist, an error of type missing-org is signaled.

## See-Also:

`*ORG*`
DEFORG
FIND-ORG
MAKE-ORG
MISSING-ORG
ORG
ORG-DESIGNATOR
REGISTER-ORG
