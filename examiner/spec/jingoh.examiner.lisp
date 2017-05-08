(defpackage :jingoh.examiner.spec
  (:use :cl :jingoh :jingoh.examiner :jingoh.org :jingoh.tester))
(in-package :jingoh.examiner.spec)
(setup :jingoh.examiner)

(requirements-about EXAMINE
		    :around
		    (let(*break-on-finish*)
		      (call-body)))

;;;; Description:
; Examine requirements then print result.

#+syntax
(EXAMINE org &key subject ((:verbose *verbose*) *verbose*)
	 ((:vivid *print-vivid*)*print-vivid*)) ; => result

;;;; Arguments and Values:

; org := org-designator, otherwise error
#?(examine 0) :signals missing-org

; subject := subject-designator, otherwise error
#?(let((*org* (make-org))
       *issues*)
    (examine *org* :subject 0))
:signals missing-subject

; *verbose* := (mod 3) specify verbosity of print.
; when specified 0, only summary printed.
#?(let((*org*(make-org))
       *issues*)
    (eval '(defspec(+) => 0))
    (examine *org* :verbose 0))
:outputs #.(format nil "~A NIL~%"(cl-ansi-text:green "Pass"))

; when specified 1, issues are printed when fails.
#?(let((*org*(make-org))
       *issues*)
    (eval '(defspec (+) => 0))
    (examine *org* :verbose 1))
:outputs #.(format nil "~A NIL~%"(cl-ansi-text:green "Pass"))
#?(let((*org*(make-org))
       *issues*
       *break-on-fails*)
    (eval '(defspec (+) => 1))
    (examine *org* :verbose 1))
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
    (examine *org*))
:outputs #.(format nil "NIL~A~%~A NIL~%"
		   (cl-ansi-text:green ".")
		   (cl-ansi-text:green "Pass"))

; vivid := boolean, control print colorization.

; result := nil
#?(let((*org* (make-org))
       *issues*)
    (eval '(defspec (+) => 0))
    (examine *org*))
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
    (examine :no-such-org))
:signals missing-org

; When subject is not found, an error of type missing-subject is signaled.
#?(let((*org*(make-org))
       *issues*)
    (examine *org* :subject 'no-such-subject))
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

(requirements-about *STOP-ON-FAILS*
		    :around
		    (let(*break-on-finish*)
		      (call-body)))

;;;; Description:
; Stop rest verifying when fails.
#?(let((*org*(make-org))
       (*stop-on-fails* T)
       *break-on-fails*
       *issues*)
    (eval'(defspec(+) => 1))
    (eval'(defspec(+) => 0))
    (examine *org*))
:outputs #.(format NIL "~2%; Stop to examine cause *STOP-ON-FAILS* at NIL~2%~A ~%"
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

(requirements-about *BREAK-ON-FAILS*
		    :around
		    (let(*break-on-fails*)
		      (call-body)))

;;;; Description:
; Breaks when fails
#?(let((*org*(make-org))
       (*break-on-fails* T)
       *issues*)
    (eval '(defspec (+) => 1))
    (examine *org*))
:invokes-debugger jingoh.examiner::break-on-fails
,:stream NIL

; Value type is NULL
#? *BREAK-ON-FAILS* :be-the boolean

; Initial value is NIL

;;;; Affected By:
; none

;;;; Notes:
; This is useful when test size is short,
; because ASDF make RESTART named CLEAR-CONFIGURATION-AND-RETRY.

(requirements-about *ISSUES*
		    :around
		    (let(*break-on-finish*)
		      (call-body)))

;;;; Description:
; Previous issues.
#?(let((*org*(make-org))
       *break-on-fails*
       *issues*)
    (eval '(defspec (+) => 1))
    (examine *org*)
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
