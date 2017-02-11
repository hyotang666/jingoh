(in-package :cl-user)
(defpackage :design-for-org(:use :cl :jingoh.org jingoh.reader jingoh.tester))
(in-package :design-for-org)

(jingoh:setup :jingoh.org)
(musam:enable)

(requirements-about main-api
		    :around (let((*org*(make-org)))
			      (call-body)))

#|
Like CL:DEFPACKAGE, we can define new org with deforg
|#
#?(deforg :test)
:satisfies org-p

#|
Unlike CL:DEFPACKAGE, org name is only symbol is acceptable.
|#
#?(deforg "test") :signals error
, :lazy t

#|
Like CL:PACKAGE, current org is in special symbol *org*
|#
#?*org* :satisfies org-p

#|
Like CL:DEFPACKAGE, definition does not change current org.
|#
#?(progn (deforg :test)
	 (eq :test (org-name *org*)))
=> NIL

#|
Like CL:PACKAGE, in order to change current org, we need to in.
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
ADD-REQUIREMENT is stable
|#
#?(let((*org*(make-org)))
     (add-requirement 0)
     (add-requirement 1)
     (map-requirements #'identity))
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
