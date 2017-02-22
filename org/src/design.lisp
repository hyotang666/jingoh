(in-package :cl-user)
(defpackage :design-for-org(:use :cl :jingoh.org jingoh.reader jingoh.tester))
(in-package :design-for-org)

(jingoh:setup :jingoh.org)
(musam:enable)

(requirements-about main-api
		    :around (let((*org*(make-org)))
			      (call-body))
		    :after (requirements-about main-api))

#+syntax(deforg name)
#+BNF(name := symbol)

#|
We can define new org with deforg, it likes CL:DEFPACKAGE but without options.
|#
#?(deforg :test) :be-the ORG

#|
Org name is only symbol is acceptable, it differs CL:DEFPACKAGE.
|#
#?(deforg "test") :signals error
, :lazy t

#|
Current org is in special symbol *org*, it likes CL:PACKAGE.
|#
#?*org* :be-the ORG

#|
Definition does not change current org, it same with CL:DEFPACKAGE.
|#
#?(progn (deforg :test)
	 (eq :test (org-name *org*)))
=> NIL

#|
In order to change current org, we need to in, it likes CL:IN-PACKAGE.
|#
#?(progn (deforg :test)
	 (in-org :test)
	 *org*)
:satisfies #`(& (org-p $a)
		(eq :test (org-name $a)))

#|
Current subjects is in psuedo special symbol *subjects*.
|#
#?*subjects* :expanded-to (ORG-CURRENT-SUBJECTS *ORG*)

#|
And default subjects is (nil).
|#
#?*subjects*
=> (NIL)
,:test equal

#|
To change current subject, we need to use REQUIREMENTS-ABOUT.
|#
#?(requirements-about requirements-about) => (REQUIREMENTS-ABOUT)
,:test equal

#|
Sometimes, we need to specify same behavior for some operators.
(e.g. APPEND and NCONC.)
In such cases, we can use COMMON-REQUIREMENTS-ABOUT with :AS keyword.
|#

#?(common-requirements-about (append nconc) :as op)
=> (APPEND NCONC)
,:test equal

#|
When missing :AS, an error will be signaled.
|#
#?(common-requirements-about (append nconc)) :signals ERROR

(requirements-about org-object)

#|
org-specifications's data structure is not specified.
|#
#?(org-specifications *org*) => #.implementation-dependent

#|
so, when you want to add item to org, you should use add-requirement.
|#
#?(let((o(make-org)))
    (add-requirement 0 o)) => 0

#|
When second argument is omitted, *ORG* is used.
|#
#?(let((*org*(make-org)))
    (add-requirement 0)
    (org-requirements-count *org*))
=> 1

#|
ADD-REQUIREMENT has stable order.
|#
#?(let((o(make-org)))
     (add-requirement 0 o)
     (add-requirement 1 o)
     (map-requirements #'identity t o))
=> (0 1)
, :test equal

#|
when argument is not org, an error will be signaled.
|#
#?(add-requirement 0 :not-org) :signals not-org

(requirements-about miscellaneous)

#|
some operation to every requirement, you can use map-requirements.
map-requirements return new list like CL:MAPCAR
But unlike CL:MAPCAR, map-requirements has default arguments.
It is *subjects* and *org*.
|#
#?(let((*org*(make-org)))
     (add-requirement 0)
     (map-requirements #'1+))
=> (1)
, :test equal

#|
Unlike CL:MAPCAR, MAP-REQUIREMENTS does not accept some orgs.
|#
#?(map-requirements #'+ T (make-org)(make-org)):signals error
, :lazy t

#+syntax(do-requirements(var &optional subject org return)&body body)
#+BNF (var := [requirement-var || (requirement-var subject-var)]
       requirement-var := symbol
       subject-var := symbol
       subject := subject-designator-generate-form
       org := org-generate-form
       return := return-form
       body := forms)

#|
if you does not need result list, (because of side effect)
you can use do-requirements which like CL:DOLIST.
|#
#?(let((*org*(make-org)))
     (add-requirement 0)
     (do-requirements(s)
       (print s)))
:outputs "
0 "

#|
do-requirements's syntax is similar with CL:DOLIST.
It means you can specify return form.
|#
#?(let((o(make-org))
	(result))
     (add-requirement 0 o)
     (do-requirements(s t o result)
       (push s result)))
=> (0)
, :test equal

#|
Like CL:HASH-TABLE-COUNT you can count how many requirements in org
|#
#?(let((*org*(make-org)))
    (add-requirement 0)
    (org-requirements-count *org*))
=> 1
