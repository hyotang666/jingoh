# Accessors of ISSUE types

## Syntax:

(accessor arg) => result

(SETF (accessor arg) new-value) => new-value

## Arguments and Values:

accessor := (member issue-form issue-expected issue-actual issue-position test-issue-test condition-issue-message unsatisfied-clause-args)

arg := issue object

result := any lisp object

## Description:
Accepts issue object, return coresponding slots value.

## Example:

## Affected-By:

## Side-Effects:

## Notes:
Abstract accessor can accept concrete object.
E.g., ISSUE-FORM can accept UNSATISFIED-CLAUSE object, but UNSATISFIED-CALUSE-ARGS can not accept ISSUE object.
To see inheritance relationship, see "issue.T.md" .

## Exceptional-Situations:
When arg is not proper issue object, an error of type type-error is signaled.

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
