# [Structure] ORG

## Class Precedence List: (case in CLISP)

* org structure-object t

## Effective Slots:

* NAME [Type] SYMBOL
[READER] org-name

* PACKAGE [Type] PACKAGE
[READER] org-package

* CURRENT-SUBJECT [Type] SYMBOL
[ACCESSOR] org-current-subject

* OPTIONS [Type] LIST
[ACCESSOR] org-options

* SPECIFICATIONS [Type] VECTOR
[ACCESSOR] org-specifications

## Description:
ORG (stands in ORGanization) is abstract data type which manages system's specifications of requirements.

For extensibility, specification's concrete data type is not specified, but it is implemented as key-value-pair.
In this context, key is subject, value is requirements.

Subject is a symbol which names a group of requirements.
The name may represent abstract category/features, or may represent concrete functions/macros/types.

Options is options for subject.
Contains :before, :after and :around option for each requirement of subject.

For modulability, requirement's concrete data structure is not specified.
It is not part of jingoh.org, but jingoh.tester is responded to it.

## Example:

## See Also:

`*OPTIONS*`
`*ORG*`
`*SUBJECT*`
DEFORG
DELETE-ORG
FIND-ORG
IN-ORG
MAKE-ORG
MISSING-ORG
NOT-ORG
ORG-CURRENT-SUBJECT
ORG-DESIGNATOR
ORG-NAME
ORG-OPTIONS
ORG-P
ORG-PACKAGE
ORG-REQUIREMENTS-COUNT
ORG-SPECIFICATIONS
REGISTER-ORG
