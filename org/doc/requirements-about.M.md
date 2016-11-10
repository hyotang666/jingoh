# [Macro] REQUIREMENTS-ABOUT

## Syntax:

(REQUIREMENTS-ABOUT subject &rest options) => result

## Arguments and Values:

subject := symbol

options := key value pair. see description.

result := symbol

## Description:
Like CL:IN-PACKAGE, REQUIREMENTS-ABOUT causes the subject named by subject to become the current subject, i.e. the value of `*subject*`.

option is for setup and teardown each requirement.
Valid key is (member :before :after :around).
This is almost same with CLOS method combination (but without inheritance).
:BEFORE is corespond to setup.
:AFTER is corespond to teardown.
If what you need to care is dynamic variable, :AROUND is useful.
In around body, you can use `(CALL-BODY)` which is corespond to `(CALL-NEXT-METHOD)`.

## Example:

## Affected-By:

## Side-Effects:
The psued variable `*subject*` is assigned.
The psued variable `*options*` is assigned.
If the `requirements-about` form is a toplevel form, these assignments also occurs at compile time.

## Notes:
Unlike CL:IN-PACKAGE, REQUIREMENTS-ABOUT is no need to define.
It is like CL:DEFGENERIC and CL:DEFMETHOD relationships.

## Exceptional-Situations:
When subject is not symbol, an error of type type-error is signaled.

## See-Also:

`*OPTIONS*`
`*SUBJECT*`
ADD-REQUIREMENT
DELETE-SUBJECT
ORG-CURRENT-SUBJECT
ORG-OPTIONS
