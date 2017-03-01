(in-package :cl-user)
(defpackage :design-for-reporter
  (:use :jingoh.org :cl :jingoh.reporter :jingoh.reader :jingoh.tester))
(in-package :design-for-reporter)

(jingoh:setup :jingoh.reporter)

(requirements-about report)

#|
report reports org:*org* status.
|#
#?(let((*org*(make-org)))
     (defspec(+) => 1)
     (report))
:outputs "1 fail in NIL.
"

#|
reporter can customize. It is bound in *reporter*
|#
#?*reporter*
:satisfies #`(& (functionp $result)
		(eq 'default-reporter(millet:function-name $result)))

#|
to see detail, call detail.
|#
#?(let((*org*(make-org)))
     (defspec (+) => 1)
     (with-output-to-string(*standard-output*)
       (detail)))
:satisfies #`(& (stringp $result)
		(let((*package*(find-package :jingoh.tester)))
		  (issue-p (read-from-string $result))))

