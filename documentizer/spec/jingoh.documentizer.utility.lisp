(defpackage :jingoh.documentizer.utility.spec
  (:use :cl :jingoh :jingoh.documentizer.utility))
(in-package :jingoh.documentizer.utility.spec)
(setup :jingoh.documentizer.utility)

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

; symbol := symbol, otherwise errro.
#?(first-char "not symbol") :signals type-error

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

#?(x-alph-pathname #\a) => #P"X_Alph_a.html"
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
#?(let((*target-type* "lisp"))
    (x-alph-pathname #\b))
=> #P"X_Alph_b.lisp"

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

#?(target-path "example") => #P"example.html"
;;;; Arguments and Values:

; name := string, otherwise error.
#?(target-path :example) :signals type-error

; result := pathname

;;;; Affected By:
; `*target-type*`
#?(let((*target-type* "asd"))
    (target-path "example"))
=> #P"example.asd"

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

; symbols := (symbol*), otherwise error.
#?(index-chars #(foo bar bazz)) :signals type-error
#?(index-chars '("foo" "bar" "bazz")) :signals type-error

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

