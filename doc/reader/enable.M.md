# [Macro] ENABLE

## Syntax:

(ENABLE &optional (char #\?)) => result

## Arguments and Values:

char := character for dispatch macro

result := implementation dependent

## Description:
Setting dispatch macro character function to current readtable.

## Example:

## Affected-By:

## Side-Effects:
Destructively modify current readtable.

## Notes:

## See-Also:

## Exceptional-Situations:
When dispatch macro character is conflicted, restartable error of type macro-char-confliction is signaled.
