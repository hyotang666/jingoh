# [Function] DETAIL

## Syntax:

(DETAIL &key subject (org `*org*`)) => result

## Arguments and Values:

subject := symbol

org := org

result := null ; side effect!

## Description:
Print on `*STANDARD-OUTPUT*` a detailed description about subject's all issue.

If subject is T, current subject's issues are reported.
If subject is NIL, all issues in org are reported.
Otherwise, only specified subject's issues are reported.

If org is not specified `*org*` is reported.

## Example:

## Affected-By:

## Side-Effects:
Print on `*STANDARD-OUTPUT*`

## Notes:
When you want to customize DETAIL's behavior, you need to write PRINT-OBJECT with specifying type is jingoh.tester:issue.

## Exceptional-Situations:
When specified org is not found, an error of type missing-org is signaled.

## See-Also:

