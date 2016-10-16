# [Macro] DEFORG

## Syntax:

(DEFORG name) => result

## Arguments and Values:

name := symbol

result := org object

## Description:
Like CL:DEFPACKAGE, DEFORG creates an org and return the org.

Unlike CL:DEFPACKAGE, if defined-org-name already refers to an existing org, such org is superseded by new one.

## Example:

## Affected-By:
Existing orgs.

## Side-Effects:
Underlying org database status is modified.

## Notes:

## Exceptional-Situations:
When name is not symbol, an error of type type-error is signaled.

## See-Also:

`*ORG*`
DELETE-ORG
FIND-ORG
IN-ORG
MAKE-ORG
ORG
REGISTER-ORG
