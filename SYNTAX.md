# Syntax of jingoh.
## BNF

```
prompt test-form keyword result option\*

prompt := dispatch-macro-char option-number? prompt-char
dispatch-macro-char := #\#
option-number := positive-integer
prompt-char := #\? ; customizable

test-form := one-lisp-form

keyword := [ =>
           | :be-the
           | :satisfies
           | :values
           | :multiple-value-satisfies
           | :outputs
           | :output-satisfies
           | :signals
           | :invokes-debugger
           | :equivalents
           | :expanded-to ]

result := one-lisp-form

option := comma-char? option-key option-value
comma-char := #\,
option-key := [ :test
              | :lazy
              | :ignore-signals
              | :with-restarts
              | :stream
              | :timesout
              | :before
              | :after
              | :around 
	      | :comment ]
option-value := one-lisp-form
```
## basic forms
### =>
The keyword `=>` is used to specify primary return value.

```lisp
#? (+ 1 1) => 2
```
### :BE-THE
The keyword `:be-the` is used to specify type of primary return value.
Especially used when primary return value is unreadable object.

```lisp
#? #'car :be-the function
```

### :SATISFIES
The keyword `:satisfies` is used to specify primary return value satisfies the specified function.
This is used when return value is object and want to specify some slot values.

```lisp
#? #P"foo/bar/bazz" :satisfies (lambda (pathname)
                                 (& (pathnamep pathname)
                                    (equal '(:relative "foo" "bar")
                                           (pathname-directory pathname))
                                    (string= "bazz" (pathname-name pathname))))
```

### :VALUES
The keyword `:values` is used to specify every return values.

```lisp
#? (floor 1 3) :values (0 1)
```

### :MULTIPLE-VALUE-SATISFIES
The keyword `:multiple-value-satisfies` is used to specify return values satisfies the specified function.

```lisp
#? (values 1 :a) :multiple-value-satisfies (lambda (num key)
                                             (& (numberp num)
                                                (keywordp key)))
```

### :OUTPUTS
The keyword `:outputs` is used to specify output string.

```lisp
#? (princ :foo) :outputs "FOO"
```

### :OUTPUT-SATISFIES
The keyword `:output-satisfies` is used to specified output string satisfies specified function.

```lisp
#?(princ :hoge) :output-satisifies (lambda (string)
                                     (& (stringp string)
                                        (=  4 (length string))))
```

### :SIGNALS
The keyword `:signals` is used to specify signaled condition.
NOTE - Specify signal only, not actually invoke debugger.

```lisp
#? (error 'warning) :signals warning
```

### :INVOKES-DEBUGGER
The keyword `:invokes-debugger` is used to specify invokes the debugger with specified condition.

```lisp
#? (error 'warning) :invokes-debugger warning
```
If specified condition is `NIL` or `NOT`, it means never invokes debugger.

```lisp
#?(+) :invokes-debugger not
```

### :EQUIVALENTS
The keyword `:equivalents` is used to specify equivalent form.

```lisp
#? (gcd 3 (gcd 4 5)) :equivalents (gcd (gcd 3 4) 5)
```

### :EXPANDED-TO
The keyword `:expanded-to` is used to specify macroexpand-1 form.

*NOTE!* This feature is still alpha quality, so may removed in future.

```lisp
(defmacro demo (arg)
  (let ((var (gensym)))
    `(let ((,var ,arg))
       (+ ,var ,var))))
#? (demo 0) :expanded-to (let ((var 0))
                           (+ var var))
```

## options
### option syntax.
#### Regular syntax.

```lisp
#1? ; <--- Specify how many options.
(list 1 2 3) => (1 2 3)
:test equal
```

#### Comma syntax, equivalent above.

```lisp
#? (list 1 2 3) => (1 2 3)
, :test equal
```

### Conclusion about option syntax.
Comma syntax is easy to understand without knowledge about jingoh syntax,
and easy to edit.
But having 1 restriction.
With comma syntax, you can not use block comment (i.e. #|comment|#)
before comma.
Invalid example below.

```lisp
#? (list 1 2 3) => (1 2 3) #|invalid|#
, :test equal ; <--- this option will be not read.
              ;      and compiler claims READER-ERROR
              ;      as "comma is illegal outside of backquote"
```

Regular syntax is not convenient,
but it is built on top of native common lisp way.(means no restrictions.)
So it is named as "Regular".
Comma syntax is irregular, but recommended with tiny care.

### Option examples.
#### :TEST
The default of `=>` and `:EQUIVALENTS`'s test is `EQL`.
The default of `:OUTPUTS`'s test is `STRING=`.
The default of `:VALUES`'s test is `EQUAL`.
The default of `:EXPANDED-TO`'s test is `SEXP=`.
The default of `:INVOKES-DEBUGGER`'s test is `NIL`.

```lisp
#? "foo" => "foo"
,:test string=
```

*NOTE!* Smart compiler (e.g. python.) treats same literal as same object.
So example above may success without option.
In such case, you need to trick like below.

```lisp
#? (format nil "foo") => "foo"
,:test string=
```

*NOTE!* This option's meanings is different when it is used with `:INVOKES-DEBUGGER`.
In such case, the function must one argument function as (function(condition)boolean).
You can test environment (e.g. special symbols.) in it.

```lisp
#? (let ((*package* (find-package :cl-user)))
     (error "hoge"))
:invokes-debugger error
,:test (lambda (c)
         (declare (ignore c))
         (eq *package* (find-package :cl-user)))
```
#### :STREAM
The default is `*STANDARD-OUTPUT*`.

```lisp
#? (princ :foo *error-output*) :outputs "FOO"
, :stream *error-output*
```

When :stream specified with NIL, it means ignore any outputs.

```lisp
#? (princ :foo) => :FOO
, :stream NIL
```

#### :IGNORE-SIGNALS

```lisp
#? (signal 'warning) => NIL
, :ignore-signals warning
```

When :ignore-signals specified with NIL, it means no condition handling occurs.
This is useful to test `WARN`.

```lisp
;;;; BAD EXAMPLE 1

#? (warn "test")
:outputs "WARNING: test
"
,:stream *error-output*
```
The test above fails as `WARNING-WAS-SIGNALED` since jingoh checks it.

```lisp
;;;; BAD EXAMPLE 2

#? (warn "test")
:outputs "WARNING: test
"
,:stream *error-output*
,:ignore-signals warning
```
The test above also fails because warning will be muffled, no output occur.

```lisp
;;;; GOOD EXAMPLE

#?(warn "test")
:outputs "WARNING: test
"
,:stream *error-output*
,:ignore-signals nil
```

#### :WITH-RESTARTS
Specify additional test for restart.
If specified restart is not found, it is issued.

```lisp
#? (warn "test") :signals warning
, :with-restarts muffle-warning
```

You can specify some restarts at once.

```lisp
#? (cerror "test" "dummy") :signals error
, :with-restarts (continue abort)
```

#### :LAZY
Like `EVAL-WHEN` option.

```lisp
#? (my-macro but ill formed) :signals program-error
, :lazy T ; <--- in order to not compiled.
#? (defvar *foo* 3)
, :lazy NIL ; <--- in order to eval when compile time. Otherwise...
#? *foo* => 3 ; <--- compiler claims "*FOO* is not defined".
```

#### :TIMEOUT
Specify timeout second.
The default is 1.

### Available keyword & option combinations

| key                            | acceptable options |
| -----------------------   | ----------- |
| =>                            | :ignore-signals :timeout :lazy :stream :test |
| :values                    | :ignore-signals :timeout :lazy :stream :test |
| :equivalents                    | :ignore-signals :timeout :lazy :stream :test |
| :outputs                     | :ignore-signals :timeout :lazy :stream :test |
| :be-the                    | :ignore-signals :timeout :lazy :stream |
| :satisfies                     | :ignore-signals :timeout :lazy :stream |
| :multiple-value-satisfies | :ignore-signals :timeout :lazy :stream |
| :output-satisfies         | :ignore-signals :timeout :lazy :stream |
| :signals                    | :ignore-signals :timeout :lazy :with-restarts |
| :invokes-debugger            | :ignore-signals :timeout :lazy :with-restarts :test |
| :expanded-to                    | :ignore-signals :timeout :stream :test |

## expert
### IMPLEMENTATION-DEPENDENT
Return value is ignored.
Tests only no condition signaled, and no output.

```Lisp
#? (lisp-implementation-type) => implementation-dependent
```

### UNSPECIFIED
Always success.

```lisp
#? (pathname :foo/bar/bazz) => unspecified
```

### BEFORE, AFTER, AROUND
Like clos, specify setup, teardown or both.

### COMMENT
Additional comment for the issue.

#### For setup.

```lisp
#?(princ :hoge) :outputs "FUGAHOGE"
,:before (princ :fuga)
```
#### For teardown.

```lisp
#? (princ :hoge) :outputs "HOGEFUGA"
,:after (princ :fuga)
```

*NOTE!* - In order to ensure teardown, we use `UNWIND-PROTECT`.
This means return value is test form's one, not teardown form's one.

```lisp
#? (princ :hoge) => :HOGE
,:after (princ :fuga)
,:stream nil
```

#### For both.

```lisp
#? a => 1
,:around (let ((a 1)) (call-body))
```
Consider `CALL-BODY` is CLOS's `CALL-NEXT-METHOD`.

### Global option.
If each test needs same option, you can specify such option as global option.
To specify global option, you need to specify option as option of `REQUIREMENTS-ABOUT`.

```lisp
(requirements-about EXAMPLE :before (princ :foo))
#?(princ :bar) :outputs "FOOBAR"
#?(princ :bazz) :outputs "FOOBAZZ"
```

Of course you can shadow it.

```lisp
#?(princ :hoge) :outputs "FUGAHOGE"
,:before (princ :fuga) ; <--- Specify local option to shadow global option.
```
## Appendix
### &
Consider test like below.

```lisp
#? "hoge" :satisfies (lambda (string)
                       (and (stringp string)
                            (= 4 (length string))))
```

If test is failed, we could not get enough information about issue, because only we can know is just "AND is failed".
In such cases, we can use `&` macro.

```lisp
#? "hog" :satisfies (lambda (string)
                      (& (stringp string)
                         (= 4 (length string))))
;; =>
#S(UNSATISFIED-CLAUSE
   :FORM (= 4 (LENGTH STRING))
   :EXPECTED T
   :ACTUAL NIL
   :LINE NIL
   :ARGS (4 3))
```

*NOTE!* - `&` macro works fine only if clause is function.
Otherwise you can not get enough information especially about ARGS.

```lisp
#? "hog" :satisfies (lambda (string)
                      (& (stringp string)
                         (LET ((length (length string))) ; <--- Special operator.
                           (= 4 length))))
=>
#S(UNSATISFIED-CLAUSE
   :FORM (LET ((LENGTH(LENGTH STRING)))
           (= 4 LENGTH))
   :EXPECTED T
   :ACTUAL NIL
   :LINE NIL
   :ARGS NIL)
```
