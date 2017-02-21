(defpackage :jingoh.generator.spec (:use :cl :jingoh :jingoh.generator)
  (:import-from :jingoh.generator
		#:%generate-asd)
  )
(in-package :jingoh.generator.spec)
(setup :jingoh.generator.spec)

(requirements-about GENERATE)

#|[Generic-Function] GENERATE
Generate spec template.
|#

#| Syntax:
(GENERATE arg &key) => result
|#

#| Argument Precedence Order:
arg
|#

#| Method signature:

(GENERATE (SYMBOL SYMBOL) &KEY SYSTEM)
When symbol is keyword, call GENERATE with (asdf:find-system SYMBOL).
Otherwise generate SYMBOL's spec template.
If spec file already exists, file contents is appended.
Otherwise new file is made.

(GENERATE (FORM LIST) &KEY)
FORM must be CL:DEFPACKAGE expression.

(GENERATE (SYSTEM ASDF/SYSTEM:SYSTEM) &KEY)
|#

#| Arguments and Values: |#

;; arg := (or symbol keyword (cons (eql cl:defpackage)t) asdf:system)

;; result := null

#| Affected By: |#

#| Side-Effects: |#
; May create file.
#| Notes: |#

#| Exceptional-Situations: |#

(requirements-about %GENERATE-ASD)

#|[Function] %GENERATE-ASD
Print asd file format for spec.
|#
#?(%generate-asd :hoge '((defpackage fuga (:use))))
:output-satisfies
#`(with-input-from-string(in $result)
    (& (string= (read-line in)"; vim: ft=lisp et")
       (equal (read in)
	      '(in-package :asdf))
       (equal (read in)
	      '#.(let((*package*(find-package :jingoh.generator)))
		   (read-from-string
		     "(defsystem :hoge.test
				 :depends-on (:jingoh \"hoge\")
				 :components ((:file \"fuga\"))
				 :perform (test-op(o c)
					    (uiop:symbol-call :jingoh :report)))")))))

#| Syntax:
(%GENERATE-ASD system forms) => result
|#

#| Arguments and Values: |#

;; system := asdf-system-designator

;; forms := cl:defpackage-form*

;; result := null

#| Affected By: |#

#| Side-Effects: |#
; outputs to *standard-output*

#| Notes: |#

#| Exceptional-Situations: |#

