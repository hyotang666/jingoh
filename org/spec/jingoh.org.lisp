(defpackage :jingoh.org.spec (:use :cl :jingoh :jingoh.org))
(in-package :jingoh.org.spec)
(setup :jingoh.org)

#| main apis for light users. |#

(requirements-about DEFORG :around (let((jingoh.org::*orgs*(make-hash-table)))
				     (call-body)))
;;;; Description:
; Define new ORGanization object for your system.
#?(deforg :demo) :be-the ORG

#+syntax
(DEFORG name) ; => result

;;;; Arguments and Values:

; name := symbol, otherwise ERROR.
#?(deforg "ERROR") :signals TYPE-ERROR
,:lazy T

; result := org

;;;; Affected By:
; none

;;;; Side-Effects:
; Underlying org database is modified.

;;;; Notes:
; DEFORG does not affect current org.
; It is same like CL:DEFPACKAGE.
#?(progn (deforg :demo)
	 (org-name *org*))
:satisfies #`(not(eq :demo $name))

;;;; Exceptional-Situations:

(requirements-about IN-ORG :around (let((jingoh.org::*orgs*(make-hash-table))
					(*org* (make-org)))
				     (call-body)))


;;;; Description:
; Modify current org.
#?(progn (deforg :demo)
	 (princ(org-name(in-org :demo)))
	 (princ(org-name *org*)))
:outputs "DEMODEMO"

#+syntax
(IN-ORG name) ; => result

;;;; Arguments and Values:

; name := org-designator, otherwise error.
#?(in-org 0) :signals TYPE-ERROR
,:lazy T

; result := org
#?(progn (deforg :demo)
	 (in-org :demo))
:be-the ORG

;;;; Affected By:
; Underlying org database.

;;;; Side-Effects:
; Modify *ORG*.

;;;; Notes:

;;;; Exceptional-Situations:
; When specified org does not exist, an error is signaled.
#?(in-org :no-such-org) :signals MISSING-ORG

(requirements-about REQUIREMENTS-ABOUT :around (let((*org*(make-org)))
						 (call-body)))

;;;; Description:
; Declare current subject of current org.

#+syntax
(REQUIREMENTS-ABOUT subject &rest option*) ; => result
#?(requirements-about +) => (+)
,:test equal

;;;; Arguments and Values:

; subject := subject-designator. ; see subject-designator below.
; Otherwise error.
#?(requirements-about 0) :signals TYPE-ERROR
,:lazy T

; option := keyword value pair.
; Supportted keys are unspecified.
; It is not jingoh.org's respond.

; result := List which includes current subjects.

;;;; Affected By:
; *ORG*

;;;; Side-Effects:
; Modify *ORG* current-subject slot and options slot.
#?(let(acc)
    (deforg :demo)
    (in-org :demo)
    (push (org-current-subjects *org*)acc)
    (push (org-options *org*)acc)
    (requirements-about + :key :value)
    (push (org-current-subjects *org*)acc)
    (push (org-options *org*)acc)
    (nreverse acc))
=> ((NIL) NIL (+) (:KEY :VALUE))
,:test equal

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about COMMON-REQUIREMENTS-ABOUT :around (let((*org*(make-org)))
							(call-body)))

;;;; Description:
; Declare current subjects of current org.
#?(common-requirements-about (first car) :as op)
=> (FIRST CAR)
,:test equal

#+syntax
(COMMON-REQUIREMENTS-ABOUT (&rest subject*) &rest option* &key (as (error "keyword parameter :as is required."))) ; => result

;;;; Arguments and Values:

; subject := subject-designator. Described later. 
; Otherwise error.
#?(common-requirements-about (0 "hoO") :as op) :signals ERROR
,:lazy T

; option := key value pair.
; Supported key value pair is unspecified,
; but :AS is required. See below.

; as := symbol. Otherwise error.
#?(common-requirements-about(first car) :as 0) :signals TYPE-ERROR
,:lazy T
; specify alias for common subjects.

; result := List which includes specified subjects.

;;;; Affected By:
; *ORG*

;;;; Side-Effects:
; Modify *ORG* current-subject slot and options slot.
#?(let(acc)
    (deforg :demo)
    (in-org :demo)
    (push (org-current-subjects *org*)acc)
    (push (org-options *org*)acc)
    (common-requirements-about (car first) :as op)
    (push (org-current-subjects *org*)acc)
    (push (org-options *org*)acc)
    (nreverse acc))
=> ((NIL) NIL (CAR FIRST) (:AS OP))
,:test equal

;;;; Notes:

;;;; Exceptional-Situations:

#| internal apis for hackers. |#

(requirements-about ORG)

; Represents ORGanization which specify system's requirements.

;;;; Class Precedence List: (case in CLISP)
; org structure-object t

;;;; Effective Slots:

; NAME [Type] SYMBOL
; [READER] org-name

; PACKAGE [Type] PACKAGE
; [READER] org-package

; CURRENT-SUBJECTS [Type] CONS
; [ACCESSOR] org-current-subjects

; OPTIONS [Type] LIST
; [ACCESSOR] org-options

; SPECIFICATIONS [Type] VECTOR
; [ACCESSOR] org-specifications

;;;; Notes:

(requirements-about ORG-P)

;;;; Description:
; When arg is ORG, return T, otherwise NIL.
#?(org-p (make-org)) => T
#?(org-p 0) => NIL

#+syntax
(ORG-P #:arg0) ; => result

;;;; Arguments and Values:

; arg0 := any lisp object.

; result := boolean

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about MAKE-ORG)

;;;; Description:
; Make new org object.
#?(make-org) :be-the org

#+syntax
(MAKE-ORG &key (#:name nil) (#:package *package*) (#:current-subjects `(nil)) (#:options nil) 
(#:specifications
 (make-array 0 :fill-pointer 0 :adjustable t :element-type 'spec))) ; => result

;;;; Arguments and Values:

; name := symbol represents organization name. Otherwise MAY error.
#?(make-org :name 0) => unspecified

; package := package which organization in. Otherwise MAY error.
#?(make-org :package 0) => unspecified

; current-subjects := cons which includes current subjects. Otherwise MAY error.
#?(make-org :current-subjects 0) => unspecified

; options := list which includes options for current subjects. Otherwise MAY error.
#?(make-org :options 0) => unspecified

; specifications := vector which includes specifications. Otherwise MAY error.
#?(make-org :specifications 0) => unspecified

; result := ORG

;;;; Affected By:
; *package* when :PACKAGE is not specified.

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about ORG-NAME :around (let((o(make-org)))
				       (call-body)))

;;;; Description:
; Return org name.
#?(org-name o) => NIL

#+syntax
(ORG-NAME #:arg0) ; => result

;;;; Arguments and Values:

; arg0 := org, Otherwise error.
#?(org-name 0) :signals TYPE-ERROR
,:lazy t
,:ignore-signals warning

; result := symbol represents org name.

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about ORG-PACKAGE :around (let((org(make-org)))
					  (call-body)))

;;;; Description:
; return package which org in.
#?(org-package org) :be-the package

#+syntax
(ORG-PACKAGE #:arg0) ; => result

;;;; Arguments and Values:

; arg0 := org, otherwise error.
#?(org-package 0) :signals TYPE-ERROR
,:lazy t
,:ignore-signals warning

; result := package

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about ORG-CURRENT-SUBJECTS :around(let((org(make-org)))
						  (call-body)))

;;;; Description:
; return current subjects.
#?(org-current-subjects org) => (NIL)
,:test equal

#+syntax
(ORG-CURRENT-SUBJECTS #:arg0) ; => result

#+setf
(SETF (ORG-CURRENT-SUBJECTS #:ARG1) #:ARG0) ; => new-value

;;;; Arguments and Values:

; arg0 := org, otherwise error.
#?(org-current-subjects 0) :signals TYPE-ERROR
,:lazy t
,:ignore-signals warning

; result := cons which includes current subjects.

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about ORG-OPTIONS :around (let((org(make-org)))
					  (call-body)))

;;;; Description:
; return current options.
#?(org-options org) => NIL

#+syntax
(ORG-OPTIONS #:arg0) ; => result

#+setf
(SETF (ORG-OPTIONS #:ARG1) #:ARG0) ; => new-value

;;;; Arguments and Values:

; arg0 := org, otherwise error.
#?(org-options 0) :signals TYPE-ERROR
,:lazy T
,:ignore-signals warning

; result := list which includes options

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about ORG-SPECIFICATIONS :around (let((org(make-org)))
						 (call-body)))

;;;; Description:
; return vector which includes specifications.
#?(org-specifications org) => #()
,:test equalp
#+syntax
(ORG-SPECIFICATIONS #:arg0) ; => result

#+setf
(SETF (ORG-SPECIFICATIONS #:ARG1) #:ARG0) ; => new-value

;;;; Arguments and Values:

; arg0 := org, otherwise error.
#?(org-specifications 0) :signals TYPE-ERROR
,:lazy t
,:ignore-signals warning

; result := vector which includes specifications.

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about REGISTER-ORG :around (let((jingoh.org::*orgs*(make-hash-table)))
					   (call-body)))

;;;; Description:
; register specified org into underlying org database.
#?(register-org :hoge (make-org)) :be-the org

#+syntax
(REGISTER-ORG name org) ; => result

;;;; Arguments and Values:

; name := symbol, otherwise error.
#?(register-org 0 (make-org)) :signals TYPE-ERROR
,:lazy t
,:ignore-signals warning

; org := Org, otherwise error.
#?(register-org :hoge 0) :signals TYPE-ERROR
,:lazy t
,:ignore-signals warning

; result := org

;;;; Affected By:
; underlying org database.

;;;; Side-Effects:
; modify underlying org database.
#?(let(acc)
    (push (find-org :hoge nil)acc)
    (register-org :hoge (make-org))
    (push (find-org :hoge nil) acc)
    (nreverse acc))
:satisfies #`(destructuring-bind(existp org)$result
	       (& (null existp)
		  (org-p org)))
;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about FIND-ORG)

;;;; Description:
; Find org from underlying org database.

#+syntax
(FIND-ORG org-designator &optional (errorp t)) ; => result

;;;; Arguments and Values:

; org-designator := org-designator, describe later. Otherwise nil.
#?(find-org 0 nil) => NIL

; errorp := boolean, when specified T (the default) if specified org is not found, an error is signaled.
#?(find-org :no-such-org) :signals MISSING-ORG
#?(find-org :no-such-org nil) => NIL

; result := org when found, if org is not found and errorp specified NIL, NIL is returned, otherwise error was signaled.

;;;; Affected By:
; underlying org database.

;;;; Side-Effects:
; none

;;;; Notes:
; if ORG-DESIGNATOR is NIL, org which name is NIL is returned.
#?(find-org nil)
:satisfies #`(& (org-p $result)
		(null (org-name $result)))

;;;; Exceptional-Situations:

(requirements-about DELETE-ORG :around (let((jingoh.org::*orgs*(make-hash-table)))
					 (call-body)))

;;;; Description:
; delete specified org from underlying org database.
#?(let(acc)
    (push (find-org :hoge nil)acc)
    (register-org :hoge (make-org :name :hoge))
    (push (find-org :hoge) acc)
    (delete-org :hoge)
    (push (find-org :hoge nil) acc)
    (nreverse acc))
:satisfies #`(destructuring-bind(existp1 hoge existp2)$result
	       (& (null existp1)
		  (org-p hoge)
		  (null existp2)))

#+syntax
(DELETE-ORG org-designator) ; => result

;;;; Arguments and Values:

; org-designator := org-designator, described later.

; result := T

;;;; Affected By:
; Underlying org database.

;;;; Side-Effects:
; Modify underlying org database.

;;;; Notes:
; Return value is always T, even if any org is deleted.

;;;; Exceptional-Situations:

(requirements-about ORG-REQUIREMENTS-COUNT)

;;;; Description:
; Return number of requirements.
#?(let((org(make-org)))
    (princ(org-requirements-count org))
    (add-requirement 'subject 0 org)
    (princ(org-requirements-count org)))
:outputs "01"

#+syntax
(ORG-REQUIREMENTS-COUNT org) ; => result

;;;; Arguments and Values:

; org := org, otherwise error.
#?(org-requirements-count 0) :signals NOT-ORG

; result := non negative integer.

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about MAP-REQUIREMENTS :around(let((*org*(make-org :current-subjects '(subject))))
					      (add-requirement 'subject 0)
					      (add-requirement 'subject 1)
					      (call-body)))

;;;; Description:
; Apply FUNCTION and collect its return value.
#?(map-requirements #'1+)
=> (1 2)
,:test equal

#+syntax
(MAP-REQUIREMENTS function &optional (subject t) (org *org*)) ; => result

;;;; Arguments and Values:

; function := (function(requirement)T)

; subject := subject designator, described later.

; org := org, otherwise error.
#?(map-requirements #'1+ t 0) :signals NOT-ORG

; result := list

;;;; Affected By:
; *org* when ORG is not specified.
; Org curret subject when subject is not specified.

;;;; Side-Effects:
; none

; Notes:

;;;; Exceptional-Situations:
; When org is not found, an error is signaled.
#?(map-requirements #'1+ :no-such-subject) :signals MISSING-SUBJECT

(requirements-about DO-REQUIREMENTS :around(let((*org*(make-org :current-subjects '(subject))))
					     (add-requirement 'subject 0)
					     (add-requirement 'subject 1)
					     (call-body)))

;;;; Description:
; Iterate forms for each requirements.
#?(do-requirements(req)
    (princ req))
:outputs "01"

#+syntax
(DO-REQUIREMENTS (var &optional (subject-designator t) (org '*org*) return) &body body) ; => result

;;;; Arguments and Values:

; var := When symbol, it is bound by each requirement.
; When list, its first element is symbol which is bound by each requirment,
; and, second element is symbol which is bound by such requirement's sbuject.
#?(do-requirements((req sub))
    (format t "~&~S ~S"req sub))
:outputs "0 SUBJECT
1 SUBJECT"

; subject-designator := subject designator, described later.

; org := org generate form. when such form does not generate org, an error is signaled.
#?(do-requirements(req t 0)
    (princ req))
:signals NOT-ORG
,:lazy t
,:ignore-signals warning

; return := return value generate form.
#?(do-requirements(req t *org* (princ :end))
    (princ req))
:outputs "01END"

; inside return form, VAR is able to seen but it is bound by NIL.
#?(do-requirements(req t *org* (princ req))
    (princ req))
:outputs "01NIL"

; any values are able to be returned.
#?(let((sum 0))
    (do-requirements(req t *org* (values sum 1 2 3))
      (incf sum req)))
:values (1 1 2 3)

; body := implicit progn.
; Body wrapped implicit block named nil, so you can return.
#?(do-requirements(req t *org* (princ :never))
    (if(zerop req)
      (return :hoge)
      (princ req)))
=> :hoge

; CL:DECLARE can appear in top of BODY.
#?(do-requirements(req)
    (declare(type integer req))
    (princ req))
:outputs "01"

; result := value which return form generates.
; The default is nil.
#?(do-requirements(req)
    (+ req))
=> NIL

;;;; Affected By:
; *org* when org is not specified.
; org-current-subjects when subject is not specified, or specified as T.
; org-specifications when subject is specified by nil.

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about ADD-REQUIREMENT :around(let((*org*(make-org)))
					     (call-body)))

;;;; Description:
; add requirement into org.
#?(add-requirement 'subject 0) => 0

#+syntax
(ADD-REQUIREMENT subject requirement &optional (org *org*)) ; => result

;;;; Arguments and Values:

; subject := symbol, otherwise error.
#?(add-requirement "subject" 0) :signals TYPE-ERROR

; requirement := any lisp object. unspecified.

; org := org, otherwise error.
#?(add-requirement 'subject :value 0) :signals NOT-ORG

; result := requirement

;;;; Affected By:
; *org* when org is not specified.

;;;; Side-Effects:
; specified org object is destructively modified.
#?(progn (princ(org-requirements-count *org*))
	 (add-requirement 'subject 0)
	 (princ(org-requirements-count *org*)))
:outputs "01"

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about DELETE-SUBJECT :around (let((*org*(make-org :current-subjects '(subject)
								:specifications (make-array 2 :fill-pointer 2 :adjustable t :element-type 'jingoh.org::spec :initial-contents (list (jingoh.org::spec 'subject 0)(jingoh.org::spec 'another 1))))))
					     (call-body)))

;;;; Description:
; delete subject from org.
#?(progn (do-requirements((req sub)nil)
	   (declare(ignore req))
	   (princ sub))
	 (delete-subject 'subject)
	 (do-requirements((req sub)nil)
	   (declare(ignore req))
	   (princ sub)))
:outputs "SUBJECTANOTHERANOTHER"

#+syntax
(DELETE-SUBJECT subject-designator &optional (org *org*)) ; => result

;;;; Arguments and Values:

; subject-designator := subject-designator, describe later

; org := org, otherwise error
#?(delete-subject 'subject 0) :signals TYPE-ERROR

; result := T
#?(delete-subject 'subject) => T

;;;; Affected By:
; *org* when org is not specified.
; Org-current-subject when subject is not specified,
; or specified by T.

;;;; Side-Effects:
; org specifications is modified destructively.

;;;; Notes:
; return value is always T even no subject is deleted.
#?(delete-subject :no-such-subject) => T

;;;; Exceptional-Situations:

(requirements-about org-designator)
;;;; Description:
; Represents org.
; (or (and symbol (not boolean)) org)
#?(typep T 'org-designator) => NIL
#?(typep NIL 'org-designator) => NIL
#?(typep :hoge 'org-designator) => T
#?(typep (make-org) 'org-designator) => T

;;;; Compound Type Specifier Kind:
; none

;;;; Compound Type Specifier Syntax:
; none

;;;; Compound Type Specifier Arguments:
; none

;;;; Compound Type Specifier Description:
; none

(requirements-about subject-designator)
;;;; Description:
; Represents subject.
; Symbol or boolean.
; When T, it represents current subject.
; When NIL, it represents all subject.

;;;; Compound Type Specifier Kind:
; none

;;;; Compound Type Specifier Syntax:
; none

;;;; Compound Type Specifier Arguments:
; none

;;;; Compound Type Specifier Description:
; none

#| special variables |#

(requirements-about *ORG*)

; Current org.

; Value type is ORG
#? *ORG* :be-the ORG

; Initial value is #<ORG NIL>

;;;; Affected By:
; IN-ORG

;;;; Notes:

#| conditions |#

(requirements-about NOT-ORG)

#|[Condition] NOT-ORG |#

;;;; Class Precedence List: (case in CLISP)
; not-org type-error error serious-condition condition standard-object t

;;;; Effective Slots:

; API [Type] SYMBOL
; [READER] api
; Which API got not org value.

; $DATUM [Type] T
; [READER] type-error-datum

; $EXPECTED-TYPE [Type] T
; [READER] type-error-expected-type

;;;; Notes:

(requirements-about MISSING)

#|[Condition] MISSING |#
; Super condition.

;;;; Class Precedence List: (case in CLISP)
; missing error serious-condition condition standard-object t

;;;; Effective Slots:

; API [Type] SYMBOL
; [READER] api
; Which API missing.

; DATUM [Type] T
; [READER] datum

;;;; Notes:

(requirements-about MISSING-ORG)

#| [Condition] MISSING-ORG |#

;;;; Class Precedence List: (case in CLISP)
; missing-org missing error serious-condition condition standard-object t

;;;; Effective Slots:

; API [Type] SYMBOL
; [READER] api

; DATUM [Type] T
; [READER] datum

;;;; Notes:

(requirements-about MISSING-SUBJECT)

#| [Condition] MISSING-SUBJECT |#

;;;; Class Precedence List: (case in CLISP)
; missing-subject missing error serious-condition condition standard-object t

;;;; Effective Slots:

; API [Type] SYMBOL
; [READER] api

; DATUM [Type] T
; [READER] datum

;;;; Notes:

(requirements-about API)

#| [Generic-Function] API |#

;;;; Description:
; return api which is signaled condition.

#+syntax
(API clos::object) ; => result

;;;; Argument Precedence Order:
; clos::object

;;;; Method signature:
; (API (CLOS::OBJECT MISSING))
; (API (CLOS::OBJECT NOT-ORG))

;;;; Arguments and Values:

; object := (or missing not-org)

; result := symbol

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about DATUM)

#| [Generic-Function] DATUM |#

;;;; Description:
; inherited from CL:TYPE-ERROR, see hyperspec.

#+syntax
(DATUM clos::object) ; => result

;;;; Argument Precedence Order:
; clos::object

;;;; Method signature:
#+elt(DATUM (CLOS::OBJECT MISSING))

;;;; Arguments and Values:

; object := MISSING

; result := T

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:
;|#
