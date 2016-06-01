# [Macro] REQUIREMENTS-ABOUT

## Syntax:

(REQUIREMENTS-ABOUT subject) => result

## Arguments and Values:

subject := symbol

result := symbol

## Description:
Like CL:IN-PACKAGE, REQUIREMENTS-ABOUT causes the subject named by subject to become the current subject, i.e. the value of `*subject*`.

## Example:

## Affected-By:

## Side-Effects:
The psued variable `*subject*` is assigned.
If the `requirements-about` form is a toplevel form, this assignment also occurs at compile time.

## Notes:
Unlike CL:IN-PACKAGE, REQUIREMENTS-ABOUT is no need to define.
It is like CL:DEFGENERIC and CL:DEFMETHOD relationships.

## Exceptional-Situations:
When subject is not symbol, an error of type type-error is signaled.

## See-Also:

`*SUBJECT*`
ADD-REQUIREMENT
DELETE-SUBJECT
ORG-CURRENT-SUBJECT
