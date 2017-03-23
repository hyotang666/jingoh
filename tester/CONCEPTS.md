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
It is jingoh.examiner or jingoh.org who responds it respectively.

## Terms

### REQUIREMENT
REQUIREMENT is internal datatype which represents specifications of requirement for operator's behavior.

### ISSUE
ISSUE is abstract datatype which contains information about issue of implementation of requiements.

