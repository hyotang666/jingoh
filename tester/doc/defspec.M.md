# [Macro] DEFSPEC

## Syntax:

(DEFSPEC &body body) => result

## Arguments and Values:

body := arguments for MAKE-REQUIREMENT

result := current subject

## Description:
DEFSPEC creates tester which generates issue list when CHECKed, then restore it to current org as current subject's requirement.
Returns current subject.

## Example:
```lisp
(defspec (+) => 0)
=> NIL ; anonymous subject.
```

## Affected-By:
`jingoh.org:*org*` `jingoh.org:*subject*`

## Side-Effects:
`jingoh.org:*org*` is modified.

## Notes:

## Exceptional-Situations:
When syntax is invalid, an error of type syntax-error is signaled.

## See-Also:

&
?
IMPLEMENTATION-DEPENDENT
MAKE-REQUIREMENT
UNSPECIFIED
