# [Function] REGISTER-ORG

## Syntax:

(REGISTER-ORG name org) => result

## Arguments and Values:

name := symbol

org := org object

result := org object

## Description:
Creates new name-org-mapping.
If specified org name already refers to an existing org, such org is superseded by new one.
This is Underlying procedure of DEFORG.

## Example:

## Affected-By:

## Side-Effects:
Underlying org database status is modified.

## Notes:

## Exceptional-Situations:
When name is not symbol, an error of type type-error is signaled.
When org is not org, an error of type not-org is signaled.

## See-Also:

`*ORG*`
DEFORG
DELETE-ORG
FIND-ORG
IN-ORG
MAKE-ORG
NOT-ORG
ORG
ORG-NAME
