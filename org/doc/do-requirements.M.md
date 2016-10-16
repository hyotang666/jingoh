# [Macro] DO-REQUIREMENTS

## Syntax:

(DO-REQUIREMENTS (var &optional (subject-designator T) (org `'*org*`) return)
  &body body) => result

## Arguments and Values:

var := symbol ; not be evaluated.

subject-designator := subject-designator ; be evaluated only once.

org := org ; be evaluated only once.

return := one lisp form ; be evaluated only once.

body := any lisp forms ; be evaluated.

result := implicit progn

## Description:
Like CL:DOLIST, DO-REQUIREMENTS iterates over the requirements of an org's specifications.

DO-REQUIREMENTS evaluates org, which should prodece an org object.
It then evaluates subject-designator which should produce an subject designator, to retrieve subject from org's specifications.
It then executes the body once for each requirement in the subject, in order in the implecit progn, with var bound to the requirement.
Finally return is evaluated.

When subject is T, current subject (i.e. `*subject*`) 's requirements are iterated.
When subject is NIL, all requirements in specifications are iterated.
Otherwise specified one's requirements are iterated.

Unlike CL:DOLIST, at the time RETURN is processed, VAR is invisible.

Like CL:DOLIST, an implicit block named NIL surrounds DO-REQUIREMENTS.
CL:RETURN may be used to terminate the loop immediately without performing any further iterations, returning zero or more values.

Unlike CL:DOLIST, body form does not wrapped with CL:TAGBODY.

The scope of the binding of VAR does not include the ORG, SUBJECT-DESIGNATOR, nor RETURN.

DO-REQUIREMENTS establishes a new binding of VAR on each iteration.

## Example:

## Affected-By:

## Side-Effects:

## Notes:
CL:DECLARE for VAR is allowed at top of BODY.

## Exceptional-Situations:
When org does not produce org object, an error of type not-org is signaled.
When specified subject is not found, an error of type missing-subject is signaled.

## See-Also:

MAP-REQUIREMENTS
MISSING-SUBJECT
NOT-ORG
SUBJECT-DESIGNATOR
