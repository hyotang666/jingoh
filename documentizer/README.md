# JINGOH.DOCUMENTIZER 6.6.1
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
* SBCL/2.1.7
* CCL/1.12.1
* CLISP/2.49
* ECL/21.2.1
* Allegro/10.1
* CMUCL/21D
* ABCL/1.8.0 ; See below.

### Known issue.
#### ABCL
ABCL has the [issue](https://github.com/armedbear/abcl/issues/405).
One test failed but all features are works fine.

## Installation

