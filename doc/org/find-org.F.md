# [Function] FIND-ORG

## Syntax:

(FIND-ORG org-designator &optional (errorp t)) => result

## Arguments and Values:

org-designator := org-designator

errorp := boolean

result := org object

## Description:
Like CL:FIND-PACKAGE, FIND-ORG searches org specified by org-designator.
Returns org object which has the name specified by org-designator.
When org-designator is already org, it is returned.

## Example:

## Affected-By:
Underlying org database status.

## Side-Effects:

## Notes:

## Exceptional-Situations:
When specified org is not found, and errorp is T (the default), an error of type missing-org is signaled.

## See-Also:

DEFORG
DELETE-ORG
MAKE-ORG
MISSING-ORG
ORG
ORG-DESIGNATOR
ORG-NAME
REGISTER-ORG
