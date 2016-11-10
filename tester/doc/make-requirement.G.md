# [Generic-Function] MAKE-REQUIREMENT

## Syntax:

(MAKE-REQUIREMENT form key expected &rest params) => result

## Argument Precedence Order:

form key expected

## Method signature:

* (MAKE-REQUIREMENT (TEST-FORM T) (KEY (EQL =>)) (EXPECTED T) &REST PARAMETERS)
* (MAKE-REQUIREMENT (TEST-FORM T) (KEY (EQL =>)) (EXPECTED (EQL #:IMPLEMENTATION-DEPENDENT)) &REST PARAMETERS)
* (MAKE-REQUIREMENT (TEST-FORM T) (KEY (EQL =>)) (EXPECTED (EQL #:UNSPECIFIED)) &REST PARAMETERS)
* (MAKE-REQUIREMENT (TEST-FORM T) (KEY (EQL :OUTPUT)) (EXPECTED T) &REST PARAMETERS)
* (MAKE-REQUIREMENT (TEST-FORM T) (KEY (EQL :VALUES)) (EXPECTED T) &REST PARAMETERS)
* (MAKE-REQUIREMENT (TEST-FORM T) (KEY (EQL :SATISFIES)) (EXPECTED T) &REST PARAMETERS)
* (MAKE-REQUIREMENT (TEST-FORM T) (KEY (EQL :MULTIPLE-VALUE-SATISFIES)) (EXPECTED T) &REST PARAMETERS)
* (MAKE-REQUIREMENT (TEST-FORM T) (KEY (EQL :SIGNALS)) (EXPECTED T) &REST PARAMETERS)
* (MAKE-REQUIREMENT (TEST-FORM T) (KEY (EQL :INVOKE-DEBUGGER)) (EXPECTED T) &REST PARAMETERS)
* (MAKE-REQUIREMENT (TEST-FORM T) (KEY (EQL :INVOKE-DEBUGGER)) (EXPECTED NULL) &REST PARAMETERS)

## Arguments and Values:

form := one lisp expression to test

key := (MEMBER => :signals :values :output :satisfies :multiple-value-satisfies :invoke-debugger)

expected := one lisp value which should be return value of form.

params := param\*

param := option-key option-value

option-key := (member :test :lazy :stream :ignore-warning :with-restarts :before :after :around)

option-value := one lisp value

result := internal requirement object, unspecified.

## Description:
MAKE-REQUIREMENT creates requirement tester which generates issue list when CHECKed.

### reserved-keyword

* =>
Specifying primary return value of FORM.

* :values
Specifying every return value of FORM.
NOTE! - In this case, you need to specify EXPECTED as list of every expected return values.
```lisp
(? (values 1 2) :values (1 2))
```
NOTE! - In this case, check is done by CL:EQUAL.
```lisp
;; above form is equivalant as below.
(equal (multiple-value-list(values 1 2)) (1 2))
```

* :output
Specifying FORM outputs specified string.
In this case, you may need to specify outputted stream with parameter :stream.

* :signals
Specifying FORM signals specified condition.
In this case, you can specify established restarts with parameter :with-restarts.
NOTE! - This checks only signal. If you need to specify FORM invokes debugger, you need to use keyword :invoke-debugger.

* :satisfies
Specifying FORM's primary return value satisfies specified function.
```lisp
(? 1 :satisfies integerp)
```

* :multiple-value-satisfies
Specifying FORM's every value satisfies specified function.
Specified function must accept FORM's every return values as arguments.
```lisp
(? (values 1 :a) :multiple-value-satisfies (lambda(num key)
                                             (and (numberp num)
					          (keywordp key))))
```

* :invoke-debugger
Specifying FORM invokes debugger.
Specified value is condition type which invokes debugger.
But when value is specified NIL, it means FORM never invoke debugger.
```lisp
(? (signal 'error) :invoke-debugger nil)
(? (error 'warning) :invoke-debugger warning)
(? (warning 'error) :invoke-debugger type-error)
```
You can specify established restarts with parameter :with-restarts.

### options

* :test
The default is EQL.
When return value is list, string, etc... you need to specify this.

* :lazy
When tesing the ill formed macro ensure signals error or not, you need to specify this option with true.
In some cases, one test-form must be evaluated in compile time.
In such cases, you need to specify this option with NIL explicitly.
NOTE! - Under the table, jingoh compile test form at first.
I.e. macro will be expanded.
Then meking requirement object.
Then restore it.
Function JINGOH.REPORTER:REPORT or JINGOH.REPORTER:DETAIL check requirements by calling test.

* :stream
When keyword is :output, you may need to specify outputted stream with this option.
The default is `*standard-output*`.

* :ignore-warning
When you know test form signals warning, but want to ignore it, you need to specify this option with true.

* :with-restarts
This is designed for using with :signals or :invoke-debugger key to test restart is established.
Expected restarts are specified as restart name (symbol) or list of restart names.
Not be evaluated.

* :before :after :around
This is used as setup or teardown.
FORM wrapped by such forms.
In :around, you can use CALL-BODY (like CLOS:CALL-NEXT-METHOD).

## Example:

## Affected-By:

## Side-Effects:

## Notes:

## Exceptional-Situations:

## See-Also:

&
?
CANONICALIZE
CHECK
DEFSPEC
ENCALLABLE
IMPLEMENTATION-DEPENDENT
MAKE-REQUIREMENT
RESERVED-KEYWORDS
THE-PUSH-INSTANCE-FORM
THE-STANDARD-HANDLING-FORM
UNSPECIFIED
