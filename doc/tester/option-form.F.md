# [Function] OPTION-FORM

## Syntax:

(OPTION-FORM key parameters) => result

## Arguments and Values:

key := (member :before :after)

parameters := parameter-plist usually it is MAKE-REQUIREMENT parameter named params

result := optional form's list.

## Description:
Returns listed optional form if exists.
Otherwise nil.

## Example:

## Affected-By:

## Side-Effects:

## Notes:
This is designed to be used in backquote, and be used with ,@ .

## Exceptional-Situations:

## See-Also:

CALL-BODY
CANONICALIZE
ENCALLABLE
RESERVED-KEYWORDS
THE-PUSH-INSTANCE-FORM
THE-STANDARD-HANDLING-FORM
