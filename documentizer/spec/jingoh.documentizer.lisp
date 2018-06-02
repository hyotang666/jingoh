(defpackage :jingoh.documentizer.spec
  (:import-from :jingoh.documentizer.dsl
		#:meta-data
		#:%make-meta-data)
  (:import-from :jingoh.documentizer.parse-spec
		#:make-single
		#:make-common)
  (:import-from :jingoh.documentizer
		#:%about-package
		#:%symbol-index
		#:%packages
		#:%top
		#:meta-datas<=system)
  (:use :cl :jingoh :jingoh.documentizer))
(in-package :jingoh.documentizer.spec)
(setup :jingoh.documentizer)

(requirements-about DOCUMENTIZE)

;;;; Description:
; accepts system, then generate documentation html files.

#+syntax
(DOCUMENTIZE system) ; => result

;;;; Arguments and Values:

; system := asdf system designator, otherwise error.
#?(documentize 0) :signals ASDF::FORMATTED-SYSTEM-DEFINITION-ERROR

; result := NIL

;;;; Affected By:

;;;; Side-Effects:
; create some files.

;;;; Notes:

;;;; Exceptional-Situations:
; when system is not found, error is signaled.
#?(documentize :no-such-system) :signals ASDF:MISSING-COMPONENT

(requirements-about META-DATAS<=SYSTEM)

;;;; Description:
; accepts asdf system, returns meta-data list.
#?(meta-datas<=system (asdf:find-system :jingoh.generator))
:satisfies
#`(& (listp $result)
     (every (lambda(x)
	      (typep x 'meta-data))
	    $result))
,:ignore-signals warning
,:stream NIL

#+syntax
(META-DATAS<=SYSTEM system &optional (sys-dir
                                      (asdf:system-source-directory
                                       system))) ; => result

;;;; Arguments and Values:

; system := asdf system object, otherwise error.
#?(meta-datas<=system :jingoh.generator) :signals error

; sys-dir := system source directory
; For easy testing, this is optional.

; result := list which includes meta-data object.

;;;; Affected By:
; file system.
; `*compile-verbose*` `*compile-print*` `*load-verbose*` `*load-print*`

;;;; Side-Effects:
; load some systems into lisp image.

; may outputs.

; open and read file contents.

;;;; Notes:

;;;; Exceptional-Situations:
; when spec file is not found, warning is signaled.
#?(meta-datas<=system (asdf:find-system :jingoh)) :signals warning

; in such case, return value is nil.
#?(meta-datas<=system (asdf:find-system :jingoh)) => NIL
,:ignore-signals warning
,:stream nil

(requirements-about %TOP)

;;;; Description:
; Print intermediate markdown for top.html
#?(%top (asdf:find-system :jingoh.documentizer.test))
:outputs
"# jingoh.documentizer.test
1. [PACKAGES](packages.html)
2. [SYMBOLS](symbols.html)
"

#+syntax
(%TOP system) ; => result

;;;; Arguments and Values:

; system := asdf system object, otherwise error.
#?(%top :jingoh.documentizer.test) :signals error

; result := NIL
#?(%top (asdf:find-system :jingoh.documentizer.test)) => NIL
,:stream NIL

;;;; Affected By:
; none

;;;; Side-Effects:
; Print to `*standard-output*`

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about %PACKAGES)

;;;; Description:
; accepts list of metadata, print intermediate markdown for packages html.
#?(%packages (list (%make-meta-data :name :hoge)))
:outputs
"# Packages Index
1. [HOGE](P_HOGE.html)
"

#+syntax
(%PACKAGES meta-datas) ; => result

;;;; Arguments and Values:

; meta-datas := list which includes meta-data objects, otherwise error.
#?(%packages (%make-meta-data :name :hoge)) :signals type-error
#?(%packages '(:not-meta-data)) :signals type-error

; result := NIL
#?(%packages (list (%make-meta-data :name :hoge))) => NIL
,:stream nil

;;;; Affected By:
; none

;;;; Side-Effects:
; Print to `*standard-output*`

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about %SYMBOL-INDEX)

;;;; Description:
; accepts list of meta-data and system designator, print intermediate markdown symbol index html.
#?(%symbol-index (list (%make-meta-data :specifieds '(hoge))) "system")
:outputs
"# Alphabetical Symbol Index

There is 1 symbol by system.

A | B | C | D | E | F | G | [H](X_Alph_H.html) | I | J | K | L | M | N | O | P | Q | R | S | T | U | V | W | X | Y | Z | Non-Alphabetic"

#+syntax
(%SYMBOL-INDEX meta-datas system) ; => result

;;;; Arguments and Values:

; meta-datas := list which includes meta-data objects, otherwise error.
#?(%symbol-index (%make-meta-data) :system) :signals error
#?(%symbol-index '(:not-meta-data) :system) :signals error

; system := asdf system designator, otherwise error.
#?(%symbol-index (list (%make-meta-data)) 0)
:signals ASDF::FORMATTED-SYSTEM-DEFINITION-ERROR

; result := nil
#?(%symbol-index (list (%make-meta-data :specifieds '(hoge))) "system")
=> NIL
,:stream nil

;;;; Affected By:
; none

;;;; Side-Effects:
; print `*standard-output*`

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about %ABOUT-PACKAGE)

;;;; Description:
; accepts meta-data, print intermediate markdown for pacakge html.
#?(%about-package(%make-meta-data :name :package-name
				  :doc "package documentation"
				  :exports '(symbols which extracted from |defpackage's| export option)
				  :singles (list (make-single :name 'symbols
							      :path #P"path/to/file.html"))
				  :commons (list (make-common :names '(which extracted)
							      :path #P"path/to/common/file.html"))))
:outputs
"# PACKAGE-NAME
## PACKAGE-NAME Concepts
package documentation
## PACKAGE-NAME Dictionary

* [SYMBOLS](path/to/file.html)
* [WHICH](path/to/common/file.html)
* [EXTRACTED](path/to/common/file.html)
* FROM
* |defpackage's|
* EXPORT
* OPTION
"

#+syntax
(%ABOUT-PACKAGE meta-data) ; => result

;;;; Arguments and Values:

; meta-data := meta-data object, otherwise error.
#?(%about-package :not-meta-data) :signals type-error

; result := nil
#?(%about-package(%make-meta-data :name :package-name
				  :doc "package documentation"
				  :exports '(symbols which extracted from |defpackage's| export option)
				  :singles (list (make-single :name 'symbols
							      :path #P"path/to/file.html"))
				  :commons (list (make-common :names '(which extracted)
							      :path #P"path/to/common/file.html"))))
=> NIL
,:stream nil

;;;; Affected By:
; none

;;;; Side-Effects:
; print to `*standard-output*`

;;;; Notes:

;;;; Exceptional-Situations:

