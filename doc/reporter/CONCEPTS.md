# jingoh.reporter - Situation management system

## Abstract concepts

Jingoh.reporter provides jingoh.tester:issue's printer.

Displays information about org to `*standard-output*`.

There is two ways to print issues.
REPORT and DETAIL.

REPORT print just summary of all issues.
This is intended to be used by asdf:test-system.

On the other hand, DETAIL is intended to be used for print detail of issues.
E.g. which form has unexpected behavior, what is expected result, what is actual returned value, unexpeted error occurs?, how about unexpected warning? etc...

DETAIL helps you to make your products robust.

## Terms

## Symbol categories

* main api
DETAIL REPORT

* customising
`*REPORTER*` DEFAULT-REPORTER

## Background

## Dictionary

* `*REPORTER*`
* DEFAULT-REPORTER
* DETAIL
* REPORT
