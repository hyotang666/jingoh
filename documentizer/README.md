# JINGOH.DOCUMENTIZER 6.0.0
## What is this?
Jingoh extension: Import/Convert specification documentation.

## Usage
### To HTML.

```lisp
(jingoh.documentizer:documentize :your-system-name)
```

### To GITHUB.WIKI MD

```lisp
(jingoh.documentizer:github-wiki :your-system-name)
```

### IMPORT documentation

```lisp
(jingoh.documentizer:import :your-system-name)
```

### COMPILE spec to lisp file.

```lisp
(jingoh.documentizer:compile :your-system-name)
```

## From developer

### Product's goal

### License
MIT
### Developed with
SBCL
### Tested with
SBCL/1.5.8
CCL/1.11.5
ECL/16.1.3

### Known issue.
CLISP fails to load due to 3BMD does not load.

## Installation

