# [Function] ADD-REQUIREMENT

## Syntax:

(ADD-REQUIREMENT requirement &optional (org `*org*`)) => result

## Arguments and Values:

requirement := unspecified

org := org

result := requirement

## Description:
Adding requirement to org's specifications as under current subject (i.e. `*subject*`).
When current subject is not found in specifications, new key-value-pair is created automatically.
Returns added requirement.
Added order is stable.

## Example:

## Affected-By:

## Side-Effects:
Specified org's specifications are modified.

## Notes:
For modulability, requirement's concrete data type is not specified.
It is not part of jingoh.org.
Jingoh.org just provides container and operator which manupilates container only.

## Exceptional-Situations:
When org is not org object, an error of type not-org is signaled.

## See-Also:

`*ORG*`
`*SUBJECT*`
DO-REQUIREMENTS
MAP-REQUIREMENTS
NOT-ORG
ORG-CURRENT-SUBJECT
ORG-SPECIFICATIONS
REQUIREMENTS-ABOUT
