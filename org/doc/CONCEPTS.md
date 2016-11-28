# jingoh.org - Jingoh's background database system

## Abstract concepts
Jingoh.org provides jingoh's background database system.
Provides abstract container type named ORG and its operators.

## Terms
* ORG
Stands in ORGanization.
It manages all REQUIREMENTs of SUBJECTs about your system.
You can refer it as test-suite which usual test framework has.

* REQUIREMENT
What the operator must/should/may behaves.
You can refer it as test which usual test framework has.

* SUBJECT
Target of REQUIREMENT.
```management hierarchy
+-----+
| ORG | ; e.g. system
+-----+
   |
+----------------------------------------+
|+----------+    			 | ; called "specifications"
|| SUBJECTs | ; e.g. functions or macros |
|+----------+				 |
|   |					 |
|+--------------+			 |
|| REQUIREMENTS | ; i.e. behaviors       |
|+--------------+			 |
+----------------------------------------+
```

## Background

## Symbol categories

* main apis
DEFORG IN-ORG REQUIREMENTS-ABOUT

* data structure org
MAKE-ORG - constructor
ORG - type name
ORG-CURRENT-SUBJECT - accessor
ORG-NAME - reader
ORG-P - predicate
ORG-PACKAGE - reader
ORG-SPECIFICATIONS - accessor

* operators for orgs
DELETE-ORG FIND-ORG ORG-REQUIREMENTS-COUNT REGISTER-ORG

* operators for requirements
ADD-REQUIREMENT DELETE-SUBJECT DO-REQUIREMENTS MAP-REQUIREMENTS

* (psued) variables
`*ORG*` `*SUBJECT*`

* conditions
API DATUM - readers
MISSING MISSING-ORG MISSING-SUBJECT NOT-ORG - conditions

* types
ORG-DESIGNATOR - type
SUBJECT-DESIGNATOR - type

## Dictionary

* `*ORG*`
* `*SUBJECT*`
* ADD-REQUIREMENT
* API
* DATUM
* DEFORG
* DELETE-ORG
* DELETE-SUBJECT
* DO-REQUIREMENTS
* FIND-ORG
* IN-ORG
* MAKE-ORG
* MAP-REQUIREMENTS
* MISSING
* MISSING-ORG
* MISSING-SUBJECT
* NOT-ORG
* ORG
* ORG-CURRENT-SUBJECT
* ORG-DESIGNATOR
* ORG-NAME
* ORG-P
* ORG-PACKAGE
* ORG-REQUIREMENTS-COUNT
* ORG-SPECIFICATIONS
* REGISTER-ORG
* REQUIREMENTS-ABOUT
* SUBJECT-DESIGNATOR
