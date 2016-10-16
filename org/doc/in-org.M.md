# [Macro] IN-ORG

## Syntax:

(IN-ORG name) => result

## Arguments and Values:

name := symbol

result := org object

## Description:
Like CL:IN-PACKAGE, IN-ORG causes the org named by name to become the current org, i.e. the value of `*org*`.

## Example:

## Affected-By:

## Side-Effects:
The variable `*org*` is assigned.
If the `in-org` form is a toplevel form, this assignment also occurs at compile time.

## Notes:

## Exceptional-Situations:
If no such org already exists, an error of type missing-org is signaled.

## See-Also:

`*ORG*`
DEFORG
DELETE-ORG
FIND-ORG
MAKE-ORG
MISSING-ORG
ORG
ORG-NAME
REGISTER-ORG
