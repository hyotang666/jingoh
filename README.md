# JINGOH 0.0.0 - DSL to notate specification, rather than test framework.

* Current lisp world
There are many test frameworks, e.g. 5am, prove, etc...

* Issues
Such frameworks looks like too much test.

* Proposal
Test first development is important.
But you should never forget it is one kind of early optimization.
Jingoh provides DSL to notate specification, rather than test framework.
Once you writes your library's specifications of requirements design with jingoh, such file works as test.
Additionaly, it will be enough to be used as tutolials for end users.

## Example
```lisp
(defpackage :demo(:use :cl :jingoh))
(in-package :demo)

(deforg :demo)
(in-org :demo)
(named-readtables:in-readtable jingoh:syntax)

(requirements-about +)

;;;; when no argument applied, returns 0.
#?(+) => 0

;;;; when one argument is applied, returns it.
#?(+ 1) => 1

;;;; When some arguments is applied, adds it.
#?(+ 1 2 3) => 6

;;;; Works fine with float.
#?(+ 0.5 0.5) => 1.0

;;;; unless acceptable arguments comes,
;;;; an error of type type-error is signaled.
#?(+ 1 nil) :signals type-error
```

## From developer

* Product's goal - ?
* License - MIT
* Supported implementation CLISP, CCL, ECL, SBCL

