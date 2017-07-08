## Abstract concepts
Jingoh provides the features to notate specification, rather than test framework.
Once you writes your library's specifications with jingoh, such file works as test.
Additionaly, it will be enough to be used as tutolials for end users.

## Packages
Package JINGOH responds to be as interface.
JINGOH's external symbols are main features for light users.
JINGOH's internal symbols are inherited from other modules.

System (same as package) JINGOH.ORG, JINGOH.TESTER, JINGOH.EXAMINER, JINGOH.READER are modules.

## Extensions
Jingoh provides some extensions.

### Jingoh.generator
Provides the features to generate test template.

### Jingoh.documentizer
Provides the features to convert test file to html, or markdown for github wiki.

### Jingoh.parallel
Provides the feature to run test in parallel.

## NOTE
Jingoh compiles test form at run time.
Its advantage is recallability.
When test code is not modified but system source code,
you does not reload test file (i.e. does not need to evaluate ASDF:TEST-SYSTEM).
Just reload system then evaluate JINGOH:EXAMINE.
JINGOH:EXAMINE can control which test should be evaluated. (the default is all)

Disadvantage is higher running cost.

