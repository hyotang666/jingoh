(defpackage :jingoh.parallel.spec
  (:use :cl :jingoh #:jingoh.parallel))
(in-package :jingoh.parallel.spec)
(setup :jingoh.parallel)

(requirements-about PEXAMINE)

;;;; Description:
; Run tests with parallel.

#+syntax
(PEXAMINE org &key subject ((:verbose *verbose*) *verbose*)
	  ((:vivid *print-vivid*) *print-vivid*)
	  (cores (get-number-of-processors))) ; => result

;;;; Arguments and Values:

; org := org-designator, otherwise error. See jingoh.org spec.

; subject := subject-designator, otherwise error. See jingoh.org spec.

; *verbose* := (mod 3). See jingoh.examiner spec.

; *print-vivid* := boolean. See jingoh.tester spec.

; cores := (and integer (satisfies plusp)), otherwise error.

; result := NIL

;;;; Affected By:
; `jingoh.examiner:*break-on-fails*` `jingoh.examiner:*stop-on-fails*`
; `jingoh.examiner:*break-on-finish*` `jingoh.tester:*print-vivid*`

;;;; Side-Effects:
; Print `*standard-output*`

;;;; Notes:
; Behavior is not completely same with jingoh.examiner:EXAMINE.
; Differences are...
; * Progress is printed subject by subject.
; * Progress is not printed when `jingoh.examiner:*break-on-fails*` or `jingoh.examiner:*stop-on-fails*` specified T.

;;;; Exceptional-Situations:

