(defpackage :jingoh.reader.spec
  (:use :cl :jingoh :jingoh.reader :jingoh.tester))
(in-package :jingoh.reader.spec)
(setup :jingoh.reader.spec)

(requirements-about ENABLE)

;;;; [Macro] ENABLE

#| Description: Set dispatch macro wich specified character to *readtable*. |#
#?(let((*readtable*(copy-readtable nil)))
    (values (get-dispatch-macro-character #\# #\?)
	    (enable)
	    (get-dispatch-macro-character #\# #\?)))
:multiple-value-satisfies
#`(& (null $1)
     $2
     $3)

#+syntax
(ENABLE &optional (char #\?)) ; => result

;;; Arguments and Values:

#| char := form generates character. |#
;; When CHAR does not generate character, an error is signaled.
#?(enable 0) :signals error
,:ignore-signals warning

#| result := implementation dependent. |#

#| Affected By: *readtable* |#

#| Side-Effects: May modify *readtable* state. |#

#| Notes: |#

#| Exceptional-Situations: |#
;; When specified dispatch macro character is already used,
;; an error of type MACRO-CHAR-CONFLICTION is signaled with
;; restart named REPLACE.
#?(let((*readtable*(copy-readtable nil)))
    (enable #\*))
:signals MACRO-CHAR-CONFLICTION
,:with-restarts REPLACE

(requirements-about REPLACE-MACRO-CHARACTER)

;;;; [Function] REPLACE-MACRO-CHARACTER

#| Description: Short hand for CL:SET-DISPATCH-MACRO-CHARACTER. |#

#+syntax
(REPLACE-MACRO-CHARACTER char) ; => result

;;; Arguments and Values:

#| char := character, otherwise error. |#
#?(replace-macro-character 0) :signals TYPE-ERROR

#| result := implementation-dependent |#

#| Affected By: none |#

#| Side-Effects: Modify *readtable* state. |#

#| Notes: |#

#| Exceptional-Situations: |#

(requirements-about |#?reader|)

;;;; [Function] |#?reader|

#| Description: Dismatch macro function for making DEFSPEC form. |#
#?(let((*readtable* (copy-readtable nil)))
    (enable)
    (with-input-from-string(in "#?(+) => 0")
      (read in)))
=> (EVAL-WHEN(:COMPILE-TOPLEVEL :LOAD-TOPLEVEL)
     (DEFSPEC (+) => 0 :POSITION 2))
,:test equal

#+syntax
(|#?reader| stream character number) ; => result

;;; Arguments and Values:

#| stream := input stream |#

#| character := dispatch macro character. |#

#| number := (or null integer) |#

#| result := DEFSPEC form. |#

#| Affected By: *read-base* *read-default-float-format* *read-eval* *read-suppress* *readtable* *read-verbose* *read-print* |#

#| Side-Effects: consume stream contents. |#

#| Notes: |#

#| Exceptional-Situations: |#

(requirements-about *READ-VERBOSE*)

#| [Variable] *READ-VERBOSE* Controls verbosity of |\#?reader| |#
#?(let((*readtable*(copy-readtable nil))
       (*read-verbose* T))
    (enable)
    (with-input-from-string(in "#?(+) => 0")
      (read in)))
:output-satisfies
#`(& (string= (format nil "~%READ: ")
	      (subseq $string 0 7))
     (equal (with-input-from-string(in $string :start 7)
	      (read in))
	    `(eval-when (:compile-toplevel :load-toplevel)
	       (defspec (+) => 0 :position 2))))
,:stream *trace-output*

;; Value type is NULL
#? *READ-VERBOSE* :be-the boolean

;; Initial value is NIL

#| Affected By: none |#

#| Notes: |#

(requirements-about *READ-PRINT*)

#| [Variable] *READ-PRINT* Control verbosity of |\#reader|. |#
#?(let((*readtable*(copy-readtable nil))
       (*read-print* T))
    (enable)
    (with-input-from-string(in "#?(+) => 0")
      (read in)))
:output-satisfies
#`(with-input-from-string(in $string)
    (& (string= "" (read-line in))
       (string= "#:TEST-FORM: (+)" (read-line in))
       (string= "#:KEYWORD: =>" (read-line in))
       (string= "#:EXPECTED: 0" (read-line in))
       (char= #\R (read-char in))
       (char= #\E (read-char in))
       (char= #\A (read-char in))
       (char= #\D (read-char in))
       (char= #\: (read-char in))
       (equal `(eval-when(:compile-toplevel :load-toplevel)
		 (defspec(+) => 0 :position 2))
	      (read in))
       (null (read in nil nil))))
,:stream *trace-output*

;; Value type is NULL
#? *READ-PRINT* :be-the boolean

;; Initial value is NIL

#| Affected By: none |#

#| Notes: |#

(requirements-about MACRO-CHAR-CONFLICTION)

#|[Condition] MACRO-CHAR-CONFLICTION Signaled when dispatch macro character is conflicted. |#

;; Class Precedence List: (case in SBCL)
;; macro-char-confliction simple-error simple-condition error serious-condition condition slot-object t

;; Effective Slots:

;; FORMAT-CONTROL [Type] T
;; [READER] simple-condition-format-control

;; FORMAT-ARGUMENTS [Type] T
;; [READER] simple-condition-format-arguments

#| Notes: |#

