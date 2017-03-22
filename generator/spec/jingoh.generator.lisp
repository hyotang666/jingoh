(defpackage :jingoh.generator.spec (:use :cl :jingoh :jingoh.generator)
  (:import-from :jingoh.generator
		#:%generate-asd
		#:%add-perform
		#:generate-header
		#:symbol-generate)
  )
(in-package :jingoh.generator.spec)
(setup :jingoh.generator.spec)

(requirements-about GENERATE)

;;;; Description:
; Generate specification template.

#+syntax
(GENERATE arg &key) ; => result

;;;; Argument Precedence Order:
; arg

;;;; Method signature:
#+signature(GENERATE (FORM LIST) &KEY)
; form must (DEFPACKAGE ...) form.
; generate each exported symbol's template.

#+signature(GENERATE (SYMBOL SYMBOL) &KEY SYSTEM)
; if symbol is keyword, recursively call generate with (asdf:find-system symbol).
; otherwise generate symbol's template.
; When system name and package is different, you need to specify system.
; If spec file already exists, file contents is appended.
; Otherwise new file is made.

#+signature(GENERATE (SYSTEM ASDF/SYSTEM:SYSTEM) &KEY)
; generate new spec directory and all spec template.

;;;; Arguments and Values:

; result := NIL

;;;; Affected By:
; file system state.

;;;; Side-Effects:
; make new spec files.

;;;; Notes:
; When system-name (i.e. keyword) is passed, system-source-file will be modified.

;;;; Exceptional-Situations:
; when system is not found, an error of type ASDF/FIND-SYSTEM:MISSING-COMPONENT is signaled.
#?(generate :no-such-system) :signals asdf:missing-component

(requirements-about %GENERATE-ASD)

;;;; Description:
; print asd file contents for spec file.
#?(%generate-asd :demo '((defpackage :hoge)))
:output-satisfies
#`(with-input-from-string(*standard-input* $string)
    (& (uiop:string-prefix-p "; vim: ft=lisp et" $string)
       (equal '(in-package :asdf)(read))
       (equal (read)
	      '(defsystem :demo.test
			  :depends-on (:jingoh "demo")
			  :components ((:file "hoge"))
			  :perform (test-op(o c)
				     (symbol-call :jingoh :examine))))))

#+syntax
(%GENERATE-ASD system forms) ; => result

;;;; Arguments and Values:

; system := asdf:system-designator otherwise error.
#?(%generate-asd 0 ())
:signals error

; forms := list which includes defpackage form. Otherwise unspecified.
#?(%generate-asd :hoge 0) => unspecified

; result := NIL
#?(%generate-asd :hoge ()) => NIL
,:stream NIL

;;;; Affected By:
; none

;;;; Side-Effects:
; print to `*standard-output*`

;;;; Notes:
; %generate-asd can work even if specified system does not exist.

;;;; Exceptional-Situations:

(requirements-about %ADD-PERFORM)

;;;; Description:
; print perform method.
#?(%add-perform :hoge)
:output-satisfies
#`(&(uiop:string-prefix-p ";; Perform method below is added by JINGOH.GENERATOR."
			  $string)
    (equal (read-from-string $string)
	 '(defmethod perform ((o test-op)(c (eql (find-system :hoge))))
	    (test-system :hoge.test))))

#+syntax
(%ADD-PERFORM name) ; => result

;;;; Arguments and Values:

; name := system name (i.e. string or symbol), otherwise unspecified.
#?(%add-perform 0) => unspecified

; result := NIL
#?(%add-perform :hoge) => NIL
,:stream NIL

;;;; Affected By:
; none

;;;; Side-Effects:
; Print to `*standard-output*`

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about GENERATE-HEADER)

;;;; Description:
; print spec file's header.
#?(generate-header :hoge)
:output-satisfies
#`(with-input-from-string(*standard-input* $string)
    (& (equal (read) '(defpackage :hoge.spec (:use :cl :jingoh :hoge)))
       (equal (read) '(in-package :hoge.spec))
       (equal (read) '(setup :hoge.spec))
       (eq :eof (read *standard-input* nil :eof))))

#+syntax
(GENERATE-HEADER package-name) ; => result

;;;; Arguments and Values:

; package-name := (or string symbol) otherwise unspecified.
#?(generate-header 0) => unspecified

; result := NIL
#?(generate-header :hoge) => NIL
,:stream NIL

;;;; Affected By:
; none

;;;; Side-Effects:
; Print to `*standard-output*`

;;;; Notes:
; Generate-header can works, evan if specified package does not exist.

;;;; Exceptional-Situations:

(requirements-about SYMBOL-GENERATE)

;;;; Description:
; Print symbol template.
#?(symbol-generate 'symbol-generate :jingoh.generator)
:outputs "(requirements-about SYMBOL-GENERATE)

;;;; Description:

#+syntax
(SYMBOL-GENERATE symbol package) ; => result

;;;; Arguments and Values:

; symbol := 

; package := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

"

#+syntax
(SYMBOL-GENERATE symbol package) ; => result

;;;; Arguments and Values:

; symbol := symbol otherwise error.
#?(symbol-generate 0 :jingoh.generator)
:signals error

; package := package-designator, otherwise error.
#?(symbol-generate 'symbol-generate 0) :signals error

; result := NIL
#?(symbol-generate 'jingoh.generator::hoge :jingoh.generator) => NIL
,:stream NIL

;;;; Affected By:

;;;; Side-Effects:
; Print to `*standard-output*`

;;;; Notes:
; When symbol does not represents function, macro, generic-function, type, class, condition, or structure, nothing outputs.
#?(symbol-generate 'hoge :jingoh.generator) :outputs ""

;;;; Exceptional-Situations:
; When specified package is not found, an error is signaled.
#?(symbol-generate 'hoge :no-such-package) :signals error

; when specified symbol is not found, an error is signaled.
#?(symbol-generate 'fuga :jingoh.generator) :signals error
