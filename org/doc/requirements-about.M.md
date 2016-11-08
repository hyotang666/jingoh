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

Options combination order is not same with method combination.
1. :before option is run.
2. :around option is run.
3. :after option is run.
And these options is NOT around subject, but each requirement of subject.
For example, when subject has 3 requirements, procedure like below.
1. :before option runs.
2. :around option which includes requirement1 runs.
3. :after option runs.
4. :before option runs.
5. :around option which includes requirement2 runs.
6. :after option runs.
7. ...so on.

## Exceptional-Situations:
When subject is not symbol, an error of type type-error is signaled.

## See-Also:

`*OPTIONS*`
`*SUBJECT*`
ADD-REQUIREMENT
DELETE-SUBJECT
ORG-CURRENT-SUBJECT
ORG-OPTIONS
