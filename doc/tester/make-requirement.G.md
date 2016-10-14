# [Generic-Function] MAKE-REQUIREMENT

## Syntax:

(MAKE-REQUIREMENT form key expected &rest params) => result

## Argument Precedence Order:

form key expected

## Method signature:

* (MAKE-REQUIREMENT (TEST-FORM T) (KEY (EQL :MULTIPLE-VALUE-SATISFIES)) (EXPECTED T) &REST PARAMETERS)
* (MAKE-REQUIREMENT (TEST-FORM T) (KEY (EQL =>)) (EXPECTED (EQL #:IMPLEMENTATION-DEPENDENT)) &REST ARGS)
* (MAKE-REQUIREMENT (TEST-FORM T) (KEY (EQL =>)) (EXPECTED (EQL #:UNSPECIFIED)) &REST PARAMETERS)
* (MAKE-REQUIREMENT (TEST-FORM T) (KEY (EQL :SATISFIES)) (EXPECTED T) &REST PARAMETERS)
* (MAKE-REQUIREMENT (TEST-FORM T) (KEY (EQL :OUTPUT)) (EXPECTED T) &REST PARAMETERS) * (MAKE-REQUIREMENT (TEST-FORM T) (KEY (EQL :VALUES)) (EXPECTED T) &REST PARAMETERS)
* (MAKE-REQUIREMENT (TEST-FORM T) (KEY (EQL :INVOKE-DEBUGGER-WITH)) (EXPECTED T) &REST PARAMETERS)
* (MAKE-REQUIREMENT (TEST-FORM T) (KEY (EQL :SIGNALS)) (EXPECTED T) &REST PARAMETERS)
* (MAKE-REQUIREMENT (TEST-FORM T) (KEY (EQL =>)) (EXPECTED T) &REST PARAMETERS)
* (MAKE-REQUIREMENT (TEST-FORM T) (KEY (EQL :NEVER-INVOKE-DEBUGGER)) (EXPECTED T) &REST PARAMETERS)

## Arguments and Values:

form := one lisp expression to test

key := (MEMBER => :signals :values :invoke-debugger-with :output :satisfies :multiple-value-satisfies :never-invoke-debugger)

expected := one lisp value which should be return value of form.

params := option-key and option-value pair\*

option-key := (member :test :lazy :stream :ignore-warning :with-restarts)

option-value := one lisp value

result := internal requirement object, unspecified.

## Description:
MAKE-REQUIREMENT creates requirement tester which generates issue list when CHECKed.

### reserved-keyword

* =>
Specifying to use primary return value of FORM to test.

* :values
Specifying to use every return value of FORM to test.
NOTE! - In this case, you need to specify EXPECTED as list of every expected return values.

* :output
Specifying to use outputting result of form to test.
In this case, you may need to specify outputted stream with parameter :stream.

* :signals
Testing specified condition is signaled or not.
In this case, you may need to specify established restarts with parameter :with-restarts.

* :satisfies
Specifying to use specified function to test.

* :multiple-value-satisfies
Specifying to use specified function to test the every return value.

* :invoke-debugger-with
Specifying the debugger will be invoked with specified condition.

* :never-invoke-debugger
Specifying the debugger will be never invoked.

### options

* :test
The default is EQL.
When return value is list, string, etc... you need to use this.

* :lazy
When tesing the ill formed macro ensure sinals error or not, you need to specify this option with true.

* :stream
When keyword is :output, you may need to specify outputted stream with this option.
The default is `*standard-output*`.

* :ignore-warning
When you know test form signals warning, but want to ignore it, you need to specify this option with true.

* :with-restarts
This is designed for using with :signals key to test restart is established.
Expected restarts are specified as restart name (symbol) or list of restart names.
Not be evaluated.
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
