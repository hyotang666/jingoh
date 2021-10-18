(defpackage :jingoh.documentizer.spec
  (:shadowing-import-from :jingoh.documentizer #:import #:compile)
  (:import-from :jingoh.documentizer
		#:meta-datas<=system
		#:meta-data
		#:%make-meta-data
		#:top
		#:packages
		#:symbol-index
		#:about-package
		#:make-single
		#:make-common
		#:with-doc-directory
		#:replace-invalid-chars
		#:escape-*
		#:first-char
		#:x-alph-pathname
		#:*x-non-alph-namestring*
		#:*target-type*
		#:target-path
		#:index-chars
		)
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
(lambda ($result)
  (& (listp $result)
     (every (lambda (x)
	      (typep x 'meta-data))
	    $result)))
,:ignore-signals (or warning
		     ;; ECL needs
		     uiop:compile-file-error)
,:stream NIL

#+syntax
(META-DATAS<=SYSTEM system &optional (sys-dir
                                      (asdf:system-source-directory
                                       system))) ; => result

;;;; Arguments and Values:

; system := asdf system object, otherwise implementation dependent condition.
#?(meta-datas<=system :jingoh.generator) :signals condition

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

(requirements-about TOP)

;;;; Description:
; Print intermediate markdown for top.html
#?(top (asdf:find-system :jingoh.documentizer.test))
:outputs
"# jingoh.documentizer.test
1. [PACKAGES](packages.html)
2. [SYMBOLS](symbols.html)
"

#+syntax
(TOP system) ; => result

;;;; Arguments and Values:

; system := asdf system object, otherwise error.
#?(top :jingoh.documentizer.test) :signals error

; result := NIL
#?(top (asdf:find-system :jingoh.documentizer.test)) => NIL
,:stream NIL

;;;; Affected By:
; none

;;;; Side-Effects:
; Print to `*standard-output*`

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about PACKAGES)

;;;; Description:
; accepts list of metadata, print intermediate markdown for packages html.
#?(packages (list (%make-meta-data :name :hoge)))
:outputs
"# Packages Index
1. [HOGE](P_HOGE.html)
"

#+syntax
(PACKAGES meta-datas) ; => result

;;;; Arguments and Values:

; meta-datas := list which includes meta-data objects, otherwise error.
#?(packages (%make-meta-data :name :hoge)) :signals type-error
#?(packages '(:not-meta-data)) :signals type-error

; result := NIL
#?(packages (list (%make-meta-data :name :hoge))) => NIL
,:stream nil

;;;; Affected By:
; none

;;;; Side-Effects:
; Print to `*standard-output*`

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about SYMBOL-INDEX)

;;;; Description:
; accepts list of meta-data and system designator, print intermediate markdown symbol index html.
#?(symbol-index (list (%make-meta-data :specifieds '(hoge))) "system")
:outputs
"# Alphabetical Symbol Index

There is 1 symbol by system.

A | B | C | D | E | F | G | [H](X_Alph_H.html) | I | J | K | L | M | N | O | P | Q | R | S | T | U | V | W | X | Y | Z | Non-Alphabetic"

#+syntax
(SYMBOL-INDEX meta-datas system) ; => result

;;;; Arguments and Values:

; meta-datas := list which includes meta-data objects, otherwise error.
#?(symbol-index (%make-meta-data) :system) :signals error
#?(symbol-index '(:not-meta-data) :system) :signals error

; system := asdf system designator, otherwise error.
#?(symbol-index (list (%make-meta-data)) 0)
:signals ASDF::FORMATTED-SYSTEM-DEFINITION-ERROR

; result := nil
#?(symbol-index (list (%make-meta-data :specifieds '(hoge))) "system")
=> NIL
,:stream nil

;;;; Affected By:
; none

;;;; Side-Effects:
; print `*standard-output*`

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about ABOUT-PACKAGE)

;;;; Description:
; accepts meta-data, print intermediate markdown for pacakge html.
#?(about-package (%make-meta-data :name :package-name
				  :doc "package documentation"
				  :exports '(symbols which extracted from |defpackage's| export option)
				  :singles #0=(list (make-single :name 'symbols
								 :path #P"path/to/file.html"))
				  :commons #1=(list (make-common :names '(which extracted)
								 :alias 'op
								 :path #P"path/to/common/file.html"))
				  :sections (append #0# #1#)))
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
(ABOUT-PACKAGE meta-data) ; => result

;;;; Arguments and Values:

; meta-data := meta-data object, otherwise error.
#?(about-package :not-meta-data) :signals type-error

; result := nil
#?(about-package (%make-meta-data :name :package-name
				  :doc "package documentation"
				  :exports '(symbols which extracted from |defpackage's| export option)
				  :singles (list (make-single :name 'symbols
							      :path #P"path/to/file.html"))
				  :commons (list (make-common :names '(which extracted)
							      :alias 'op
							      :path #P"path/to/common/file.html"))))
=> NIL
,:stream nil

;;;; Affected By:
; none

;;;; Side-Effects:
; print to `*standard-output*`

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about WITH-DOC-DIRECTORY)

;;;; Description:
; make environment to output html.

#+syntax
(WITH-DOC-DIRECTORY (pathname) &body body) ; => result

;;;; Arguments and Values:

; pathname := form generates pathname, otherwise error.
#?(with-doc-directory (0) :body) :signals type-error
,:ignore-signals warning

; body := lisp form which print markdown contents.

; result := unspecified.

;;;; Affected By:

;;;; Side-Effects:
; make file on file system.

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about REPLACE-INVALID-CHARS
		    :test string=)

;;;; Description:
; Replace invalid chars with its char-name's first character, but dot(i.e. .).

#+syntax
(REPLACE-INVALID-CHARS arg) ; => result

#?(replace-invalid-chars '*hoge*)
=> "42hoge42"
;;;; Arguments and Values:

; arg := string-designator, otherwise error.
#?(replace-invalid-chars 0) :signals type-error
#?(replace-invalid-chars "+hoge+") => "43hoge43"
#?(replace-invalid-chars #\-) => "45"
#?(replace-invalid-chars #\.) => "."

; result := string

;;;; Affected By:
; char-name's return value. Implementation dependent.

;;;; Side-Effects:
; none

;;;; Notes:
; Valid alphabet character is convert to downcase.
#?(replace-invalid-chars "&HoGe") => "38hoge"

;;;; Exceptional-Situations:

(requirements-about ESCAPE-*
		    :test string=)

;;;; Description:
; escape * character with \.

#+syntax
(ESCAPE-* arg) ; => result

#?(escape-* '*hoge*) => "\\*HOGE\\*"
;;;; Arguments and Values:

; arg := string-designator, otherwise unspecified.
#?(escape-* 0) => unspecified

; result := string

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:
; When ARG is symbol, and its PRINTed notation is |xxx|,
; return string has vertical bars.
#?(escape-* '|#hoge|) => "|#hoge|"

;;;; Exceptional-Situations:

(requirements-about FIRST-CHAR)

;;;; Description:

#+syntax
(FIRST-CHAR symbol) ; => result

#?(first-char :hoge) => #\H
#?(first-char '*hoge*) => #\*
;;;; Arguments and Values:

; symbol := symbol, otherwise implementation dependent condition.
#?(first-char "not symbol") :signals condition

; result := character

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:
; Return character is converted to uppercase.
#?(first-char '|hoge|) => #\H

;;;; Exceptional-Situations:

(requirements-about X-ALPH-PATHNAME
		    :test equal)

;;;; Description:

#+syntax
(X-ALPH-PATHNAME char) ; => result

#?(x-alph-pathname #\a)
=> ;; In ecl, literal pathname (i.e. #P"") have :version :newest.
#.(make-pathname :name "X_Alph_a" :type "html")
;;;; Arguments and Values:

; char := character which alphabet.
; when not character, unspecified.
#?(x-alph-pathname "A") => unspecified
; When character is not alphabet character, unspecified.
#?(x-alph-pathname #\あ) => unspecified
#?(x-alph-pathname #\*) => unspecified

; result := pathname

;;;; Affected By:
; `*target-type*`
#?(let ((*target-type* "lisp"))
    (x-alph-pathname #\b))
=> #.(make-pathname :name "X_Alph_b"
		    :type "lisp")

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about *X-NON-ALPH-NAMESTRING*)

;;;; Description:
; For customize.

;;;; Value type is (SIMPLE-ARRAY CHARACTER *)
#? *X-NON-ALPH-NAMESTRING* :be-the string

; Initial value is "X_NonAlpha.html"

;;;; Affected By:
; none

;;;; Notes:

(requirements-about *TARGET-TYPE*)

;;;; Description:
; To customize `TARGET-PATH`'s behavior.

;;;; Value type is (SIMPLE-ARRAY CHARACTER *)
#? *TARGET-TYPE* :be-the string

; Initial value is "html"

;;;; Affected By:

;;;; Notes:

(requirements-about TARGET-PATH
		    :test equal)

;;;; Description:

#+syntax
(TARGET-PATH name) ; => result

#?(target-path "example") => #.(make-pathname :name "example"
					      :type "html")
;;;; Arguments and Values:

; name := string, otherwise error.
#?(target-path :not-string) :signals error

; result := pathname

;;;; Affected By:
; `*target-type*`
#?(let ((*target-type* "asd"))
    (target-path "example"))
=> #.(make-pathname :name "example"
		    :type "asd")

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about INDEX-CHARS
		    :test equal)

;;;; Description:

#+syntax
(INDEX-CHARS symbols) ; => result

#?(index-chars '(foo bar bazz hoge fuga))
=> (#\B #\F #\H)
;;;; Arguments and Values:

; symbols := (symbol*), otherwise implementation dependent condition.
#?(index-chars #(not list)) :signals condition
#?(index-chars '("not" "symbol")) :signals type-error

; result := (character*)

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:
; return list is sorted.
#?(index-chars '(:c :b :d :a))
=> (#\A #\B #\C #\D)

;;;; Exceptional-Situations:

