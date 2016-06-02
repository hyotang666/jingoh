# jingoh.tester - Dsl for requirement.

## Abstract concepts
Jingoh.tester is a dsl for requirement.
It provides -
* Notations of requirement.
* Testing feature which tests implementation satisfies requirement or not.
* Abstract data types which represents issues.

For extensibility, requirement's internal representation is not specified.
Although Jingoh.tester provides operators which handle requirements.
So users does not need to know about what happen behind the black box.

For modulability, jingoh.tester does not provide report functions, and restoring database system.
It is jingoh.reporter or jingoh.org who responds it respectively.

## Terms

* REQUIREMENT
REQUIREMENT is internal datatype which represents specifications of requirement for operator's behavior.

* ISSUE
ISSUE is abstract datatype which contains information about issue of requiements.

## Background

## Operation categories

* main apis
=> ?  CHECK DEFSPEC IMPLEMENTATION-DEPENDENT UNSPECIFIED

* internal dsls
CANONICALIZE ENCALLABLE MAKE-REQUIREMENT RESERVED-KEYWORDS THE-PUSH-INSTANCE-FORM THE-STANDARD-HANDLING-FORM

* issue object names
CONDITION-ISSUE ERROR-WAS-SIGNALED ISSUE ISSUE-OF-MULTIPLE-VALUES UNEXPECTED-SUCCESS WARNING-WAS-SIGNALED WRONG-FORMAT DEBUGGER-WAS-INVOKED

* predicates
CONDITION-ISSUE-P ERROR-WAS-SIGNALED-P ISSUE-OF-MULTIPLE-VALUES-P ISSUE-P UNEXPECTED-SUCCESS-P WARNING-WAS-SIGNALED-P WRONG-FORMAT-P DEBUGGER-WAS-INVOKED-P

* accessors
CONDITION-ISSUE-MESSAGE ISSUE-ACTUAL ISSUE-EXPECTED ISSUE-FORM ISSUE-TEST

* conditions
SYNTAX-ERROR

## Dictionary

* =>
* ?
* CANONICALIZE
* CHECK
* CONDITION-ISSUE
* CONDITION-ISSUE-MESSAGE
* CONDITION-ISSUE-P
* DEFSPEC
* DEBUGGER-WAS-INVOKED
* DEBUGGER-WAS-INVOKED-P
* ENCALLABLE
* ERROR-WAS-SIGNALED
* ERROR-WAS-SIGNALED-P
* IMPLEMENTATION-DEPENDENT
* ISSUE
* ISSUE-ACTUAL
* ISSUE-EXPECTED
* ISSUE-FORM
* ISSUE-OF-MULTIPLE-VALUES
* ISSUE-OF-MULTIPLE-VALUES-P
* ISSUE-P
* ISSUE-TEST
* MAKE-REQUIREMENT
* SYNTAX-ERROR
* THE-PUSH-INSTANCE-FORM
* THE-STANDARD-HANDLING-FORM
* UNEXPECTED-SUCCESS
* UNEXPECTED-SUCCESS-P
* UNSPECIFIED
* WARNING-WAS-SIGNALED
* WARNING-WAS-SIGNALED-P
* WRONG-FORMAT
* WRONG-FORMAT-P
