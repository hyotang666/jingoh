# [Symbol-macro] `*OPTIONS*`

## Expanded-form:
```lisp
(ORG-OPTIONS *ORG*)
```

## Description:
Whatever subject is currently the value of `(org-options *org*)` is referred to as the current options.
And this symbol macro expanded into the code above, so we can regard it as a special variable.
This keeps options for current subject.

## note:
Do not confuse it as symbol.
This is just symbol-macro, so you can not access its value by CL:SYMBOL-VALUE.

## See also:

`*SUBJECT*`
REQUIREMENTS-ABOUT
