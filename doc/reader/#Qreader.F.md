# [Function] |#?reader|

## Syntax:

(|#?reader| stream character number) => result

## Arguments and Values:

stream := input-stream

character := character

number := non negative integer

result := ? form

## Description:
Reading S-expression from stream, and making ? form.

Number must be number of options for ?.

## Example:
```lisp
#?(+ 1 1) => 2
;; Need option number.
#1?(list 1 1) => (1 1) :test equal
;; if lost...
'#?(list 1 1) => (1 1) :test equal
;=> (? (list 1 1) => (1 1))
```

## Affected-By:

## Side-Effects:

## Notes:

## See-Also:

## Exceptional-Situations:

