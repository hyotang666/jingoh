# JINGOH 3.0.1
*NOTE!* Jingoh is very personal products, so use your own risk.
Other products (if any) are strongly recommended.

## What is this?
DSL to notate specification, rather than test framework.

## Notable differences from other test suites.

### Specification notation rather than test framework.
When specification is modified, test should be fixed.
Vice vaca, when a hole found in specification via test, specification should be fixed.
Idealy, both specification and test should be managed in one place.

### Readability oriented syntax rather than writablility.
In concept above, test is specification, so readability prior than writability.
Author may need writability.
Unfortunately, author is few, but the third persons.
For syntax details, see [SYNTAX.md](SYNTAX.md).

### Including specification template (i.e. as test template).
TDD begginer may not understand what should be written as test.
Thanks to CLHS, we know what should be written as specification.
For details, see [generator/README.md](generator/README.md).

### Including dribble for REPL driven development.
Most CLer will do REPL driven development.
Writting tiny codes, loading it to lisp environment, then checking it inside REPL.
If find something unexpected behavior, fixing codes, reloading it and so on.
For such users, jingoh provides special dribble repl.
Your interactions with REPL are automatically send spec file.
For details, see [generator/README.md](generator/README.md).

### Including from-spec(i.e. test)-file-to-HTML converter.
Managing both test and specification at one place allows you to never write documentation, since it is already written.
You can say "See spec/test file.", but we provide little bit prittier to html comverter.
For details, see [documentizer/README.md](documentizer/README.md).

### Including from-spec(i.e. test)-file-to-github-wiki converter.
Additionaly, we provide to github-wiki converter.
For details, see [documentizer/README.md](documentizer/README.md).

### Including from-spec(i.e. test)-file-to-lisp-documentation importer.
Additionaly, we provide lisp documentation importer.
You can refer spec documentation via CL:DOCUMENTATION.
For details, see [documentizer/README.md](documentizer/README.md).

### Customizable issue printer.
What is the best issue report?
It is issue of flavor.
We print issue as raw data object.
Thanks to CLOS, you can easily customize printer with `PRINT-OBJECT`.

### Support coloring printing.
For readability, we provide coloring printing.
In many cases, you will not feel like to customize issue printer.

### Support S-Expression-equal. (alpha quality.)
It is hard to test macro generate correct S-Expression.
Because, there is symbol which generate via `GENSYM`.
We will provide `SEXP=`. (This is alpha quality, so it is internal.)

### Support parallel testing.
Optionaly you can choose parallel testing.

### Including project skelton generator.
Battery included.
You can ignore some annoying knowledge about backward.

## How to use.
### Initialization
If your system is already made, evaluate below.

*NOTE!* You must specify your system with *keyword*.

```lisp
(asdf:load-system :jingoh.generator)
(jingoh.generator:generate :your-system)
```
In this case, jingoh will make `spec` subdirectory in your system source directory like below.

```
your-system/
  |--- your-system.asd
  |--- your-system.lisp
  |--- spec/
        |--- your-system.test.asd
        |--- your-system.lisp
```

Else if your system is not made yet, specify `:init` keyword with T.

```lisp
(jingoh.generator:generate :your-system :init t)
```
In this case, jingoh works like project skeleton generator (e.g. quick-project.).

```
your-system/
  |--- README.md
  |--- spec/
        |--- your-system.lisp
        |--- your-system.test.asd
  |--- src/
        |--- your-system.lisp
  |--- your-system.asd
```
In both cases, test is already setup.
To run test, evaluate like below.

```lisp
(asdf:test-system :your-system)
```

### Writing specifications.
Let's say your-system has function like below.

```lisp
(defun adder (num)
  "Make adder function."
  (lambda (x)
    (+ x num)))
```
To add specification of ADDER to your spec file, evaluate like below.

```lisp
(jingoh.generator:dribble :your-system)

DRIBBLE>
```
Now, you are in the dribble repl.
To append spec file with template, input :g.

```lisp
DRIBBLE> :g

>>
```

Input function name.

```
>> adder

DRIBBLE>
```
Jingoh appends spec file with template like below.

```
(requirements-about ADDER)

;;;; Description:
; Make adder function.

#+syntax
(ADDER num) ; => result

;;;; Arguments and Values:

; num := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

```
This template is designed to be same with hyper-spec.

Different from ordinary repl, dribble repl ask you each result are expected.

```lisp
DRRIBLE> (adder 1)

FUNCTION
Is it expected return type? (y or n)
```

If yes, spec file are appended.

```lisp
Is it expected return type? (y or n) y

#<CLOSURE (LAMBDA (X) :IN ADDER) {12345}>
DRIBBLE>
```

Ok, try to next test.

```lisp
DRIBBLE> (adder :not-number)

FUNCTION
Is it expected return type? (y or n)
```

Let's say you want to specify signaling an error in this case.
FUNCTION is not expected return type.

When you choice NO, it invokes ordinary deubgger.

```lisp
Is it expected return type? (y or n) n

debugger invoked on a UNEXPECTED-BEHAVIOR in thread
#<THREAD ....>:

restarts (invokable by number or by possibly-abbreviated name):
  0: [USE-VALUE] Specify expected type.
  1: [DRIBBLE  ] Return to dribble.
  2: [ABORT    ] Exit debugger, returning to top level.
```

This is ordinary debugger, so you can do anything in debugger, editting, reloading, etc.

Let's modify source like below.

```lisp
(defun adder (num)
  "Make adder function."
  (check-type num number)
  (lambda (x)
    (+ x num)))
```

After modifying and reloading your system in the debugger, select restart `DRIBBLE` to return dribble repl.

```lisp
DRIBBLE> (adder :not-number)

debugger invoded on a SIMPLE-TYPE-ERROR in thread
#<THREAD ...>:

restarts (invokable by number or by possibly-abbreviated name):
  0: [STORE-VALUE] Supply a new value for NUM.
  1: [APPEND-SPEC] Append spec, returning to dribble.
  2: [DRIBBLE    ] Return to dribble.
  3: [ABORT      ] Exit debugger, returning to top level.
```

Ok, signaling an error is expected behavior.
Let's choice restart `APPEND-SPEC`.

To exit dribble repl, input :q.

```lisp
DRIBBLE> :q

*
```

After dribble session, edit the spec file to move test code in the right place.

After editting, spec file may like below.
To write comment, use markdown syntax with semicolon escape.
To know completed syntax, see [SYNTAX.md](SYNTAX.md).
One-sentence-some-examples style is recommended.

```lisp
(requirements-about ADDER)

;;;; Description:
; Make adder function.
#?(ADDER 1) :be-the FUNCTION

; Apply number to returned function, such function return added value.
#?(FUNCALL (ADDER 1) 2) => 3

#+syntax
(ADDER num) ; => result

;;;; Arguments and Values:

; num := number which is acceptable for `CL:+`, otherwise error
#?(ADDER :NOT-NUMBER) :signals SIMPLE-TYPE-ERROR

; result := function as `(FUNCTION (NUMBER) NUMBER)`

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:
```

### Running test.
*NOTE!* Before running test, do not forget export (if it will be exported.) symbol from your system package, or import  (if it will be internals.) symbol from your system package to spec package.

```lisp
(asdf:test-system :your-system)
```

![passed example](./img/pass.png)

### Documentization
After writing your-system's spec file, if you want to get your system's html documentations, evaluate like below.

```lisp
(asdf:load-system :jingoh.documentizer)
(jingoh.documentizer:documentize :your-system)
```
Jingoh will make `doc` subdirectory in your system source directory.

If you want provide such documentations as github wiki, after clone github wiki repository, evaluate like below.

```lisp
(jingoh.documentaizer:github :your-system "path/to/your-system.wiki/")
```
Then push it.

### Parallel testing.
Normally, jingoh run tests sequentially.
If you want to run tests in parallel, modify `spec/your-system.test.asd` like below.

```lisp
(defsystem :your-system.test
  :depends-on (:jingoh.parallel ; <--- instead of :jingoh
               "your-system")
  :components ((:file "your-system"))
  :perform
  (test-op(o c)(symbol-call :jingoh.parallel ; <--- instead of :jingoh
                            :pexamine ; <--- instead of :examine
                            :your-system)))
```
*NOTE!* Parallelizing has its own overhead, so parallel testing may slower than sequential testing.
## From developer

### Product's goal
?
### License
MIT
### Tested with
* CCL/1.12
* SBCL/2.0.9
* ECL/20.4.24

### Known issue.
#### CLISP
[CLISP says](https://clisp.sourceforge.io/impnotes.html#clpp)

> The Lisp Pretty Printer implementation is not perfect yet.

JINGOH.GENERATOR depends on pretty-printer and test fails in clisp.

Other features works fine though.

## Installation
TODO
