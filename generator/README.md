# JINGOH.GENERATOR 1.2.23
## What is this?
Jingoh extension: Project skelton and test template generator and more.

## Usage
### As project skelton generator.

```lisp
(jingoh.generator:generate :your-system-name :init t)
```

### As already existing system's test skelton generator.

```lisp
(jingoh.generator:generate :your-system-name)
```

### As already existing system's test template generator.

```lisp
(jingoh.generator:generate :your-system-name :append t)
```

### As README generator/updator.

```lisp
(jingoh.generator:generate :readme :system :your-system-name)
```

### As one shot test template generator.

```lisp
(jingoh.generator:generate 'something)
```

## For REPL Driven Development.

```lisp
(jingoh.generator:dribble :your-system-name)

DRIBBLE>
```

When you input S-Expression, dribble repl evaluate it then ask you it is expected behavior.

```lisp
DRIBBLE> (+ 1 2 3)

6
Expected result? (y or n)
```

If choice YES, such form appends spec file as specification.

If choice NO, debugger is invoked.

```lisp
Expected result? (y or n) n

debugger invoked on a UNEXPECTED-BEHAVIOR in thread
#<THREAD ...>

restarts (invokable by number or by possibly-abbreviated name):
  0: [USE-VALUE] Specify expected value.
  1: [DRIBBLE  ] Return to dribble.
  2: [ABORT    ] Exit debugger, returning to top level.
```

This is ordinary debugger, so you can do anything including editting, reloading, etc.
After debugging, you can return dribbe repl with choicing restart DRIBBLE,
then re-evaluate test form.

Restart USE-VALUE is usefull for example when specify return value is IMPLEMENTATION-DEPENDENT.
Or speicfy bahavior is UNSPECIFIED.
Or relaxing signaled condition or return value type.

*NOTE* If return value is not type of (or symbol integer character), :test option is generated with EQUAL,
even if EQUAL could not test it (e.g. array).

### UNREADABLE OBJECT
When return value is unreadable object, expected type is asked.

```lisp
DRIBBLE> #'car

FUNCTION
Is it expected return type? (y or n)
```

### OUTPUT
When output is occur, it is asked.

```lisp
DRIBBLE> (princ :a)
A
Expected output? (y or n)
```

In this case, return value is also asked.

```lisp
Expected output? (y or n) y

:A
Expected result? (y or n)
```

### VALUES
When multiple values are returned, it is asked.

```lisp
DRIBBLE> (values 1 2)

1
2
Expected values? (y or n)
```

*NOTE!* If you choice NO, and select restart USE-VALUE, you must return values as LIST.

```lisp
Input expected values. >> (2 3)
```

*NOTE!* If some value is unreadable object, unfinished test code is generated.

```lisp
DRIBBLE> (values #'car #'cdr)

; Session above generate test code like bellow.

; #?(VALUES #'CAR #'CDR)
; :multiple-value-satisfies
; (LAMBDA (RESULT1 RESULT2) :TODO)
```

### WARNING
If warning is signaled, it is asked.

```lisp
DRIBBLE> (warn "test")
WARNING: test

Expected signals? #<SIMPLE-WARNING ...> (y or n)
```

In this case, return value is also asked.

```lisp
Expected signals? #<SIMPLE-WARNING ...> (y or n) y

NIL
Expected result? (y or n)
```

### ERROR
If debugger is invoked, you can choice restart APPEND-SPEC if it is expected behavior.

```lisp
DRIBBLE> (error "test")

debugger invoked on a SIMPLE-ERROR in thread
#<THREAD ...>

restarts (invokable by number or by possibly-abbreviated name):
  0: [APPEND-SPEC] Append spec, returning to dribble.
  1: [DRIBBLE    ] Return to dribble.
  2: [ABORT      ] Exit debugger, returning to top level.
```

### MACRO EXPANSION.
If test form is MACROEXPNAD-1 or MACROEXPAND, test code use :expanded-to keyword.

```lisp
DRIBBLE> (macroexpand-1 :demo)

:DEMO
Expected expansion? (y or n)
```

If you choice YES, test code like below is generated.

```lisp
#?:DEMO :expanded-to :DEMO
```

### Restrictions
Dribble could not generate syntax below.
If you want, you should write it manualy.

* :satisfies
* :output-satisfies
* :equivalents
* :invokes-debugger

Additionaly, could not generate some options like :test :before :after :around etc.

### SPECIAL COMMAND
Dribble repl has some special commands.

#### To quit.

```lisp
DRIBBLE> :q

*
```

Or type dribble form.

```lisp
DRIBBLE> (dribble)

*
```

#### To invoke template generator.

```lisp
DRIBBLE> :g

>> something

DRIBBLE>
```

#### To refer help.

```lisp
DRIBBLE> ?
```

#### EXTEND SPECIAL COMMANDS.
You can add special command like :Q, :G above with using DEFINE-SPECIAL-COMMAND.

```bnf
(define-special-command command description &body body)
```

Return value is discarded.

In special command, output to `*SPEC-OUTPUT*` is send to spec file.

## From developer

### Product's goal
?
### License
MIT

### Developed with
SBCL

### Tested with
* SBCL/2.1.10
* CCL/1.12.1
* CLISP/2.49 ; Failed.
* ECL/21.2.1
* Allegro/10.1
* CMUCL/21D
* ABCL/1.8.0 ; Failed.

### Known issue.
#### CLISP
[CLISP says](https://clisp.sourceforge.io/impnotes.html#clpp)

> The Lisp Pretty Printer implementation is not perfect yet.

JINGOH.GENERATOR depends on pretty-printer and test fails in clisp.

#### ABCL
* [Format issue](https://github.com/armedbear/abcl/issues/398)
* [Coercion issue](https://github.com/armedbear/abcl/issues/399)

## Installation

