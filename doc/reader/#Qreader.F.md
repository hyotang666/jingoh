# [Function] |#?reader|

## Syntax:

(|#?reader| stream character number) => result

## Arguments and Values:

stream := input-stream

character := character (ignored)

number := non negative integer (ignored)

result := defspec form

## Description:
Reading S-expression from stream, and making defspec form.

## Example:
```lisp
#?(+ 1 1) => 2
#?(list 1 1) => (1 1), ; <= NOTE - comma is needed when you specify additional options.
:test equal
```

## Affected-By:

## Side-Effects:

## Notes:

## See-Also:

## Exceptional-Situations:

