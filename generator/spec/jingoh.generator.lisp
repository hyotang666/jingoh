(defpackage :jingoh.generator.spec (:use :cl :jingoh :jingoh.generator)
  (:import-from :jingoh.generator
		#:%generate-asd
		#:%add-method-extension
		#:generate-header
		#:symbol-generate)
  )
(in-package :jingoh.generator.spec)
(setup :jingoh.generator)

(requirements-about GENERATE)

;;;; Description:
; Generate specification template.

#+syntax
(GENERATE arg &key) ; => result

;;;; Argument Precedence Order:
; arg

;;;; Method signature:
#+signature(GENERATE (FORM LIST) &KEY APPEND)
; Form must (DEFPACKAGE ...) form, otherwise signals an error.
; Generate each exported symbol's template.
; If spec file already exists, and APPEND is T, file is appended.
; If spec file already exists, and APPEND is NIL, nothing to do.
; If spec file does not exist, file is made.

#+signature(GENERATE (SYMBOL SYMBOL) &KEY SYSTEM INIT)
; If SYMBOL is keyword, and INTI is NIL, recursively call GENERATE with `(asdf:find-system SYMBOL)`.
; If SYMBOL is keyword, and INIT is T, recursively call GENERATE with `'init`.
; Otherwise generate symbol's template.
; When system name and package name is different, you need to specify SYSTEM.
; If spec file already exists, file contents is appended.
; Otherwise new file is made.

#+signature(GENERATE (DISPATCHER (EQL INIT)) &KEY SYSTEM)
; Generate system source (i.e. asd file), and its components (i.e. lisp file).
; Then recursively call GENERATE with `(asdf:find-system SYSTEM)`.
; Internal use.

#+signature(GENERATE (SYSTEM ASDF/SYSTEM:SYSTEM) &KEY APPEND)
; Generate new spec directory and all spec template.
; About append see above.

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
(lambda($string)
  (with-input-from-string(*standard-input* $string)
    (& (uiop:string-prefix-p "; vim: ft=lisp et" $string)
       (equal '(in-package :asdf)(read))
       (equal (read)
	      '(defsystem :demo.test
			  :version "0.0.0"
			  :depends-on (:jingoh "demo")
			  :components ((:file "hoge"))
			  :perform (test-op(o c)
				     (declare(special args))
				     (apply #'symbol-call :jingoh :examine :hoge args)))))))

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

(requirements-about %ADD-METHOD-EXTENSION)

;;;; Description:
; print method extension.
#?(%add-method-extension "hoge")
:output-satisfies
(lambda($string)
  (with-input-from-string(*standard-input* $string)
    (&(uiop:string-prefix-p (format nil "~%;; These forms below are added by JINGOH.GENERATOR.")
			    $string)
      (equal (read) '(in-package :asdf))
      (equal (read)
	     '(defmethod component-depends-on ((o test-op)(c (eql (find-system "hoge"))))
		(append (call-next-method)'((test-op "hoge.test")))))
      (equal (read)
	     '(defmethod operate :around ((o test-op)(c (eql (find-system "hoge")))
					  &rest keys &key ((:compile-print *compile-print*))
					  ((:compile-verbose *compile-verbose*))
					  &allow-other-keys)
		(flet((jingoh.args(keys)
			(loop :for (key value) :on keys :by #'cddr
			      :when (find key '(:on-fails :subject :vivid) :test #'eq)
			      :collect key :and :collect value
			      :else :when (eq :jingoh.verbose key)
			      :collect :verbose :and :collect value)))
		  (let((args(jingoh.args keys)))
		    (declare(special args))
		    (call-next-method))))))))

#+syntax
(%ADD-METHOD-EXTENSION name) ; => result

;;;; Arguments and Values:

; name := system name (i.e. string or symbol), otherwise unspecified.
#?(%add-method-extension 0) => unspecified

; result := NIL
#?(%add-method-extension "hoge") => NIL
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
(lambda($string)
  (with-input-from-string(*standard-input* $string)
    (& (equal (read) '(defpackage :hoge.spec (:use :cl :jingoh :hoge)))
       (equal (read) '(in-package :hoge.spec))
       (equal (read) '(setup :hoge))
       (eq :eof (read *standard-input* nil :eof)))))

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
:outputs "(requirements-about SYMBOL-GENERATE :doc-type function)

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

