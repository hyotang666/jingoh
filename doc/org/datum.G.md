# [Generic-Function] DATUM

## Syntax:

(DATUM object) => result

## Argument Precedence Order:

object

## Method signature:

* (DATUM (OBJECT MISSING))

## Arguments and Values:

object := specialliezed object

result := any lisp object

## Description:
Like CL:TYPE-ERROR-DATUM, DATUM returns the value which trigger the signal this condition.

## Example:

## Affected-By:

## Side-Effects:

## Notes:

## Exceptional-Situations:
When object is not speciallized one, an error of type method-call-error is signaled (i.e. no-applicable-method is called).

## See-Also:

API
MISSING
MISSING-ORG
MISSING-SUBJECT
