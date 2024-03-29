(defpackage :jingoh.generator.spec (:use :cl :jingoh :jingoh.generator)
  (:shadowing-import-from :jingoh.generator
                          #:dribble)
  (:import-from :jingoh.generator
                #:ensure-name
                #:generate-header
                #:symbol-generate
                #:parse-lambda-list)
  )
(in-package :jingoh.generator.spec)
(setup :jingoh.generator)

(requirements-about GENERATE :doc-type function)

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

#+signature(GENERATE (DISPATCHER (EQL :README)) &KEY SYSTEM)
; Generate README.md file.
; If it is already exists, update it, especially version information.

#+signature(GENERATE (DISPATCHER (EQL TEST-ASD)) &KEY SYSTEM FORMS PATH)
; Generate asd file which for test.

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

; When specify :init T, and specified name is already exists in quicklisp,
; an error is signaled.
; In such case, restart `CONTINUE` and `RENAME` are achived.
#?(generate :alexandria :init t) :signals error
,:with-restarts (jingoh.generator::rename continue)

(requirements-about GENERATE-HEADER :doc-type function)

;;;; Description:
; print spec file's header.
#?(generate-header :hoge)
:output-satisfies
(lambda ($string)
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

(requirements-about SYMBOL-GENERATE :doc-type function)

;;;; Description:
; Print symbol template.
#?(defun dummy (arg)
    "This documentation shall be embed."
    arg)
=> DUMMY
,:before (fmakunbound 'dummy)

#?(symbol-generate 'dummy :jingoh.generator.spec)
:outputs "(requirements-about DUMMY :doc-type function)

;;;; Description:
; This documentation shall be embed.

#+syntax (DUMMY ARG) ; => result

;;;; Arguments and Values:

; arg := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

"

#?(defstruct struct (slot nil :type (member :a :b :c)))
=> STRUCT
,:before (mapc #'fmakunbound '(struct-p make-struct copy-struct struct-slot (setf struct-slot)))

#?(symbol-generate 'struct :jingoh.generator.spec)
:output-satisfies
(lambda (output)
  (& (equal output
	    (format nil "(requirements-about STRUCT :doc-type STRUCTURE)

;;;; Description:
;;;; Class Precedence List: (case in ~A)
; ~{~(~A~)~^ ~}

;;;; Effective Slots:

; SLOT [Type] (MEMBER :A :B :C)~@[~A~]

;;;; Notes:

"
uiop:*implementation-type*
(mapcar #'class-name (closer-mop:class-precedence-list (find-class 'struct)))
(when (uiop:featurep :clisp)
  "
; [ACCESSOR] struct-slot")))))

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

(requirements-about ENSURE-NAME :doc-type function)

;;;; Description:

#+syntax
(ENSURE-NAME system) ; => result

#?(ENSURE-NAME :NAME) => "name"
,:test equal

;;;; Arguments and Values:

; system := asdf:system designator, otherwise error.
#?(ENSURE-NAME 0) :signals ASDF/SESSION:FORMATTED-SYSTEM-DEFINITION-ERROR

; result := string

;;;; Affected By:
; QUICKLISP provided system status.

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:
; When system name already provided by quicklisp, an error is signaled.
#?(ENSURE-NAME :ALEXANDRIA) :signals SIMPLE-ERROR
,:with-restarts (jingoh.generator::rename continue)

; In such case, restart CONTINUE is achieved.
#?(handler-bind ((error #'continue))
    (ensure-name :alexandria))
=> "alexandria"
,:test equal

; Samely, restart JINGOH.GENERATOR::RENAME is also achieved.
#?(with-input-from-string (s "new-name")
    (let ((*query-io*
            (make-two-way-stream s (make-broadcast-stream))))
      (handler-bind ((error (lambda (condition)
                              (let ((restart
                                      (find-restart 'jingoh.generator::rename condition)))
                                (when restart
                                  (let ((*error-output*
                                         (make-broadcast-stream))) ; <--- CCL need.
                                    (invoke-restart-interactively restart)))))))
        (ensure-name :alexandria))))
=> "new-name"
,:test equal

(requirements-about PARSE-LAMBDA-LIST :doc-type function)

;;;; Description:

#+syntax (PARSE-LAMBDA-LIST symbol) ; => result

;;;; Arguments and Values:

; symbol := symbol, otherwise signals condition.
#?(PARSE-LAMBDA-LIST "not symbol") :signals condition
; When symbol does not FBOUNDed, an error is signaled.
#?(PARSE-LAMBDA-LIST 'NO-SUCH) :signals condition

#+ccl ; as guard.
#?(nth-value 1 (ignore-errors (parse-lambda-list 'no-such)))
:be-the simple-error

; result := ((var typename)*)
; var := [symbol | return-spec]
; return-spec := [ "result" | "result n" | "optional" | "rest values" ]
; typename := [ string | null ]
#?(PARSE-LAMBDA-LIST 'PARSE-LAMBDA-LIST)
=> #.(or #+cmu '((SYMBOL "T") ("result" LIST))
	 '((SYMBOL NIL) ("result" NIL)))
, :test equal

;;;; Affected By:
; Proclamation of function.
; If function is DECLAIMed, specified TYPENAME is returned.
#+sbcl
#?(PARSE-LAMBDA-LIST 'jingoh.generator::OUTPUT-TO)
=> ((JINGOH.GENERATOR::PATH "PATHNAME") (JINGOH.GENERATOR::THUNK "FUNCTION")
    (JINGOH.GENERATOR::IF-EXISTS "(MEMBER :SUPERSEDE :APPEND :ERROR)") ("result" "NULL"))
, :test equal

;;;; Side-Effects:
; none.

;;;; Notes:

;;;; Exceptional-Situations:
