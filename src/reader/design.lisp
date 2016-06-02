(in-package :cl-user)
(defpackage :design-jingoh.reader(:use :cl :jingoh.reader :jingoh.tester :jingoh.org))
(in-package :design-jingoh.reader)

(jingoh:setup :jingoh.reader)

(requirements-about enable)

#|
To set dispatch macro #?, we need to evaluate enable.
|#
#?(let((*readtable*(copy-readtable)))
    (values (get-dispatch-macro-character #\# #\?)
	    (enable)
	    (get-dispatch-macro-character #\# #\?)))
:multiple-value-satisfies #`(& (null $existp1)
			       (not(null $return))
			       (& $existp2
				  (etypecase $existp2
				    (symbol (eq '|#?reader| $existp2))
				    (function (eq '|#?reader| (millet:function-name $existp2))))))

#|
can use other character
|#
#?(let((*readtable*(copy-readtable)))
    (values (get-dispatch-macro-character #\# #\!)
	    (enable #\!)
	    (get-dispatch-macro-character #\# #\!)))
:multiple-value-satisfies #`(& (null $existp1)
			       (not(null $return))
			       (& $existp2
				  (etypecase $existp2
				    (symbol (eq '|#?reader| $existp2))
				    (function (eq '|#?reader| (millet:function-name $existp2))))))


#|
when conflicts restartable condition is signaled.
|#
#1?(let((*readtable*(copy-readtable)))
     (enable #\*))
:signals macro-char-confliction
:lazy t


