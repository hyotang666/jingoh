# [Function] REPORT

## Syntax:

(REPORT &rest args) => result

## Arguments and Values:

args := (org-name\*)

result := null ; side effect!

## Description:
Call `*reporter*` function to each args.
When args is not specified, `*org*` is reported.

This function is designed to be called by asdf:test-system.

## Example:

## Affected-By:
When args is not specified, `*org*`

## Side-Effects:
Print on `*STANDARD-OUTPUT*`

## Notes:

## Exceptional-Situations:
When org is not found, an error of type missing-org is signaled.

## See-Also:
`*DEFAULT-REPORTER*`
