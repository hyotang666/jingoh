(defpackage :jingoh.examiner.spec
  (:use :cl :jingoh :jingoh.examiner :jingoh.org :jingoh.tester))
(in-package :jingoh.examiner.spec)
(setup :jingoh.examiner.spec)

(requirements-about EXAMINE)

;;;; Description:
; Examine requirements then print result.

#+syntax
(EXAMINE &key (org *org*) subject ((:verbose *verbose*) *verbose*)
	 ((:vivid *print-vivid*)*print-vivid*)) ; => result

;;;; Arguments and Values:

; org := org-designator, otherwise error
#?(examine :org 0) :signals error

; subject := subject-designator, otherwise error
#?(let((*org* (make-org))
       *issues*)
    (examine :subject 0))
:signals error

; *verbose* := (mod 3) specify verbosity of print.
; when specified 0, only summary printed.
#?(let((*org*(make-org))
       *issues*)
    (eval '(defspec(+) => 0))
    (examine :verbose 0))
:outputs #.(format nil "~A NIL~%"(cl-ansi-text:green "Pass"))

; when specified 1, issues are printed when fails.
#?(let((*org*(make-org))
       *issues*)
    (eval '(defspec (+) => 0))
    (examine :verbose 1))
:outputs #.(format nil "~A NIL~%"(cl-ansi-text:green "Pass"))
#?(let((*org*(make-org))
       *issues*)
    (eval '(defspec (+) => 1))
    (examine :verbose 1))
:outputs #.(format nil "~A in NIL~%~S ~%"
		   (cl-ansi-text:red "Fail 1 test")
		   (make-instance 'test-issue :form '(+)
				  :expected 1
				  :actual 0
				  :test 'eql))

; when specified 2 (the default.), progress is also printed.
#?(let((*org*(make-org))
       *issues*)
    (eval '(defspec (+) => 0))
    (examine))
:outputs #.(format nil "NIL~A~%~A NIL~%"
		   (cl-ansi-text:green ".")
		   (cl-ansi-text:green "Pass"))

; vivid := boolean, control print colorization.

; result := nil
#?(let((*org* (make-org))
       *issues*)
    (eval '(defspec (+) => 0))
    (examine))
=> NIL
,:stream NIL

;;;; Affected By:
; *verbose* *stop-on-fails* *break-on-fails*

;;;; Side-Effects:
; print to *standard-output*

;;;; Notes:
; If you want to modify format of ISSUE,
; you can write CL:PRINT-OBJECT which specialized by ISSUE.

;;;; Exceptional-Situations:
; When org is not found, an error of type missing-org is signaled.
#?(let((*org*(make-org))
       *issues*)
    (examine :org :no-such-org))
:signals missing-org

; When subject is not found, an error of type missing-subject is signaled.
#?(let((*org*(make-org))
       *issues*)
    (examine :subject 'no-such-subject))
:signals missing-subject

(requirements-about *VERBOSE*)

;;;; Description:
; Controls examine's verbosity.
; For detail, see EXAMINE.

; Value type is (INTEGER 0 3)
#? *VERBOSE* :be-the (mod 3)

; Initial value is 2

;;;; Affected By:
; EXAMINE

;;;; Notes:

(requirements-about *STOP-ON-FAILS*)

;;;; Description:
; Stop rest verifying when fails.
#?(let((*org*(make-org))
       (*stop-on-fails* T)
       *issues*)
    (eval'(defspec(+) => 1))
    (eval'(defspec(+) => 0))
    (examine))
:outputs #.(format NIL "Stop to examine cause *STOP-ON-FAILS*~&@NIL~%~A ~%"
		   (make-instance 'test-issue :form '(+)
				  :expected 1
				  :actual 0
				  :test 'eql)
		   (cl-ansi-text:red "1 fail"))
		   

; Value type is NULL
#? *STOP-ON-FAILS* :be-the boolean

; Initial value is NIL

;;;; Affected By:
; none

;;;; Notes:

(requirements-about *BREAK-ON-FAILS*)

;;;; Description:
; Breaks when fails
#?(let((*org*(make-org))
       (*break-on-fails* T)
       *issues*)
    (eval '(defspec (+) => 1))
    (examine))
:invokes-debugger error
,:stream NIL

; Value type is NULL
#? *BREAK-ON-FAILS* :be-the boolean

; Initial value is NIL

;;;; Affected By:
; none

;;;; Notes:
; This is useful when test size is short,
; because ASDF make RESTART named CLEAR-CONFIGURATION-AND-RETRY.

(requirements-about *ISSUES*)

;;;; Description:
; Previous issues.
#?(let((*org*(make-org))
       *issues*)
    (eval '(defspec (+) => 1))
    (examine)
    *issues*)
:satisfies #`(& (listp $result)
		(= 1 (length $result))
		(every #'issue-p $result))
,:stream NIL

; Value type is LIST
#? *ISSUES* :be-the list

; Initial value is NIL

;;;; Affected By:
; EXAMINE

;;;; Notes:
; Debug use.
