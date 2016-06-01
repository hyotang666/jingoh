# [Function] ORG-REQUIREMENTS-COUNT

## Syntax:

(ORG-REQUIREMENTS-COUNT org) => result

## Arguments and Values:

org := org

result := non negative integer

## Description:
Like CL:HASH-TABLE-COUT, ORG-REQUIREMENTS-COUNT returns the number of entries in the org's specifications.
If ORG has just been created or completely deleted, the entry count is 0.

## Example:

## Affected-By:

## Side-Effects:

## Notes:

## Exceptional-Situations:
When org is not org, an error of type not-org is signaled.

## See-Also:

ADD-REQUIREMENT
DELETE-SUBJECT
NOT-ORG
ORG
ORG-SPECIFICATIONS
