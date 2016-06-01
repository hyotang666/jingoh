# [Function] DEFAULT-REPORTER

## Syntax:

(DEFAULT-REPORTER &rest names) => result

## Arguments and Values:

names := org name

result := nil

## Description:
Prints on the `CL:*STANDARD-OUTPUT*` a description about specified org status.
When names is not specified, current org status is reported.

## Example:

## Affected-By:

## Side-Effects:
Print on `*STANDARD-OUTPUT*`

## Notes:

## Exceptional-Situations:
When specified org is not found, an error of type jingoh.org:missing-org is signaled.

## See-Also:
REPORT
