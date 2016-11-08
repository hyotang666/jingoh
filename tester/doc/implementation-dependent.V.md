# [Variable] IMPLEMENTATION-DEPENDENT

## Value Type:

SYMBOL

## Initial Value:

`#:IMPLEMENTATION-DEPENDENT`

## Description:
Represents implementation dependent bahavior.
Return value is ignored, just checking unexpected signals and unexpected outputs.
In other words, test form must be functional.

## Example:
```lisp
(? (lisp-implementation-type) => #.implementation-dependent)
=> NIL
```

## Affected By:

## Notes:
This is intended to be used with #. dispatch macro.

## See Also:

?
DEFSPEC
MAKE-REQUIREMENT
