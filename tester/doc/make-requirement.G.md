# [Generic-Function] MAKE-REQUIREMENT

## Syntax:

(MAKE-REQUIREMENT form key expected &rest params) => result

## Argument Precedence Order:

form key expected

## Method signature:

* (MAKE-REQUIREMENT (TEST-FORM T) (KEY (EQL =>)) (EXPECTED T) &REST PARAMETERS)
* (MAKE-REQUIREMENT (TEST-FORM T) (KEY (EQL =>)) (EXPECTED (EQL #:IMPLEMENTATION-DEPENDENT)) &REST PARAMETERS)
* (MAKE-REQUIREMENT (TEST-FORM T) (KEY (EQL =>)) (EXPECTED (EQL #:UNSPECIFIED)) &REST PARAMETERS)
* (MAKE-REQUIREMENT (TEST-FORM T) (KEY (EQL :OUTPUTS)) (EXPECTED T) &REST PARAMETERS)
* (MAKE-REQUIREMENT (TEST-FORM T) (KEY (EQL :VALUES)) (EXPECTED T) &REST PARAMETERS)
* (MAKE-REQUIREMENT (TEST-FORM T) (KEY (EQL :SATISFIES)) (EXPECTED T) &REST PARAMETERS)
* (MAKE-REQUIREMENT (TEST-FORM T) (KEY (EQL :MULTIPLE-VALUE-SATISFIES)) (EXPECTED T) &REST PARAMETERS)
* (MAKE-REQUIREMENT (TEST-FORM T) (KEY (EQL :SIGNALS)) (EXPECTED T) &REST PARAMETERS)
* (MAKE-REQUIREMENT (TEST-FORM T) (KEY (EQL :INVOKES-DEBUGGER)) (EXPECTED T) &REST PARAMETERS)
* (MAKE-REQUIREMENT (TEST-FORM T) (KEY (EQL :INVOKES-DEBUGGER)) (EXPECTED NULL) &REST PARAMETERS)

## Arguments and Values:

form := one lisp expression to test

key := (MEMBER => :signals :values :outputs :satisfies :multiple-value-satisfies :invokes-debugger)

expected := one lisp value which should be return value of form.

params := param\*

param := option-key option-value

option-key := (member :test :lazy :stream :ignore-signals :with-restarts :before :after :around)

option-value := one lisp value

result := internal requirement object, unspecified.

## Description:
MAKE-REQUIREMENT creates requirement tester which generates issue list when CHECKed.

### reserved-keyword

* =>
Specifying return value of FORM.
When you want to specify multiple values, specified values are must the form (values ...)

* :values
Specifying every return value of FORM.
Specified values are must the form (values ...)
```lisp
(? (values 1 2) :values (values 1 2))
```
NOTE! - In this case, check is done by CL:EQUAL.
```lisp
;; above form is equivalant as below.
(equal (multiple-value-list(values 1 2)) (1 2))
```
This is dispatched from the keyword =>.

* :outputs
Specifying FORM outputs specified string.
In this case, you can specify outputted stream with parameter :stream.

* :signals
Specifying FORM signals specified condition.
In this case, you can specify established restarts with parameter :with-restarts.
NOTE! - This checks only signal. If you need to specify FORM invokes debugger, you need to use keyword :invokes-debugger.

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

* :invokes-debugger
Specifying FORM invokes debugger.
Specified value is condition type which invokes debugger.
But when value is specified NIL, it means FORM never invoke debugger.
```lisp
(? (signal 'error) :invokes-debugger nil)
(? (error 'warning) :invokes-debugger warning)
(? (warning 'error) :invokes-debugger type-error)
```
You can specify established restarts with parameter :with-restarts.

### options

* :test
The default is EQL.
When return value is list, string, etc... you need to specify this.

* :lazy
Jingoh expression may be compiled.
(remember asdf:test-system compiles at first.)
Is such case, also test form is compiled.
It means macro will be expanded.
If you need to check, e.g. ill formed macro signals condition in expansion time, you need to specify :lazy T.

Of course jingoh expression is compiled by CL compiler.
You know CL compiler does not evaluate expression.
But sometimes you need to specify eval when compile.
In such case, you can specify this :lazy option with NIL explicitly.
Typical example below.
```lisp
#?(defvar *list* '(1 2 3))
=> *LIST*
#?(mapcar #'1+ *list*)
=> (2 3 4) , :test equal
```
Without :lazy NIL, compiler claims about unknown variable `*LIST*` in second expression.

* :stream
When keyword is :outputs, you may need to specify outputted stream with this option.
The default is `*standard-output*`.

* :ignore-signals
When you know test form signals condition, but want to ignore it, you need to specify this option with condition type.
If it is T, any conditions are ignored.

* :with-restarts
This is designed for using with :signals or :invokes-debugger key to test restart is established.
Expected restarts are specified as restart name (symbol) or list of restart names.

* :before :after :around
This is used as setup or teardown.
FORM is wrapped by such forms.
In :around, you can use CALL-BODY (like CLOS:CALL-NEXT-METHOD).
See CANONICALIZE.

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
