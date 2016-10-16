# jingoh - DSL to notate specification, rather than test framework.

## Abstract concepts
Jingoh provides the features to notate specification, rather than test framework.
Once you writes your library's specifications of requirements with jingoh, such file works as test.
Additionaly, it will be enough to be used as tutolials for end users.

## Packages
Package JINGOH responds to be as interface.
JINGOH's external symbols are main features for light users.
JINGOH's internal symbols are inherited from other modules.

System (same as package) JINGOH.ORG JINGOH.TESTER JINGOH.REPORTER JINGOH.READER are modules.

## Symbol inherited from
* jingoh.org - Background database system.
DEFORG IN-ORG REQUIREMENTS-ABOUT

* jingoh.reporter - Reporting issues features.
DETAIL REPORT

* jingoh.tester - Requirement's tester.
& ? => DEFSPEC IMPLEMENTATION-DEPENDENT UNSPECIFIED

* jingoh.reader - Special dispatch macros.
ENABLE SYNTAX

## Background
Compiles requirement form, then restores it.
When REPORT is called, specified requirements are checked and reported.

## Dictionary

* &
* ?
* =>
* DEFORG
* DEFSPEC
* DETAIL
* ENABLE
* IMPLEMENTATION-DEPENDENT
* IN-ORG
* REPORT
* REQUIREMENTS-ABOUT
* SETUP
* SYNTAX - as readtable name
* UNSPECIFIED
