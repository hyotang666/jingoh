# Predicates
```
;; graph coresponds structure inheritance.
;; concrete types <<< abstract types
wrong-format-p ---------- test-issue-p --------------+- issue-p
                          unexpected-success-p       |
		          unexpected-output-p        |
		          issue-of-multiple-values-p |
		          missing-restarts-p         |
		          unsatisfied-clause-p       |
error-was-signaled-p --+- condition-issue-p -------- +
warning-was-signaled-p |
debugger-was-invoked-p |
unmatch-condition-p ---+
```
## Syntax:

(predicate arg) => result

## Arguments and Values:

predicate := (member issue-p test-issue-p wrong-format-p unexpected-success-p unexpected-output-p issue-of-multiple-values-p missing-restart-p unsatisfied-cleause-p condition-issue-p error-was-signaled-p warning-was-signaled-p debugger-was-invoked-p unmatch-condition-p)

arg := any lisp object

result := boolean

## Description:
Accept one argument, and test it is proper object.

## Affected by:

## Example:

## Side-Effects:

## Notes:
Concrete object satisfies abstract predicate.
So, e.g., ISSUE-P returns T when WRONG-FORMAT object comes, but WRONG-FORMAT-P returns NIL when ISSUE object comes.

## Exceptional-Situations:

## See-Also:

CONDITION-ISSUE
DEBUGGER-WAS-INVOKED
ERROR-WAS-SIGNALED
ISSUE
ISSUE-OF-MULTIPLE-VALUES
MISSING-RESTARTS
TEST-ISSUE
UNEXPECTED-OUTPUT
UNEXPECTED-SUCCESS
UNMATCH-CONDITION
UNSATISFIED-CLAUSE
WARNING-WAS-SIGNALED
WRONG-FORMAT
