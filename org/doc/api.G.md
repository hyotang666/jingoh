# [Generic-Function] API

## Syntax:

(API object) => result

## Argument Precedence Order:

object

## Method signature:

* (API (OBJECT MISSING))
* (API (OBJECT NOT-ORG))

## Arguments and Values:

object := speciallized object

result := symbol

## Description:
Like CL:ARITHMETIC-ERROR-OPERATION, api returns symbol which api signals this condition.

## Example:

## Affected-By:

## Side-Effects:

## Notes:

## Exceptional-Situations:
When object is not speciallized one, an error of type method-call-error is signaled (i.e. no-applicable-method is called).

## See-Also:

DATUM
MISSING
MISSING-ORG
MISSING-SUBJECT
NOT-ORG
