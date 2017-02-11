(in-package :jingoh.reporter)

(macrolet((!(form)
	    `(RESIGNAL-BIND((MISSING-ORG()'MISSING-ORG :API 'DEFAULT-REPORTER))
	       ,form)))

  (defun default-reporter(&rest names)
    #.(Doc :jingoh.reporter "doc/default-reporter.F.md")
    (flet((REPORT(org)
	    (if(zerop(Org-requirements-count org))
	      (warn "No requirements in ~S"(Org-name org))
	      (let((count(let((count 0)
			      (*package*(Org-package org)))
			   (!(Do-requirements(Requirement nil org count)
			       (incf count (length(Check requirement))))))))
		(format t "~&~:[~D fail~:*~P in ~S~;Pass ~*~S~].~%"
			(zerop count)
			count
			(Org-name org))))))
      (if(null names)
	(REPORT *org*)
	(dolist(name names)
	  (REPORT(Find-org name)))))))

(defparameter *reporter* #'default-reporter
  #.(Doc :jingoh.reporter "doc/AreporterA.V.md"))

(macrolet((!(n form)
	    `(RESIGNAL-BIND((MISSING-ORG(C)'MISSING-ORG
			      :API ',(nth n '(REPORT DETAIL))))
	       ,form)))

  (defun report(&rest args)
    #.(Doc :jingoh.reporter "doc/report.F.md")
    (! 0(apply *reporter* args)))

  (defun detail(&key subject(org *org*))
    #.(Doc :jingoh.reporter "doc/detail.F.md")
    (let*((*org*(! 1(Find-org org)))
	  (*package*(Org-package *org*)))
      (Do-requirements(Requirement subject)
	(mapc #'print(Check requirement))))))

; subject, detail, summary
(defvar *verbose* 2)

(defparameter *break-on-fails* nil)

(defun verify(org &key subject ((:verbose *verbose*)*verbose*))
  (prog*((*org*(find-org org))
	 (*package*(Org-package *org*))
	 (current-subject)
	 (issues))
    (Do-requirements((requirement sub)subject)
      (let((result(Check requirement)))
	(setf issues(nconc issues result))
	(when(and result *break-on-fails*)
	  (format t "~&Stop to verify cause *BREAK-ON-FAILS*~&@~A"sub)
	  (go :end))
	(when(<= 2 *verbose*)
	  (unless(eq sub current-subject)
	    (setf current-subject sub)
	    (format t "~&~A"current-subject))
	  (if result
	    (write-char #\!)
	    (write-char #\.)))))
    (when(<= 1 *verbose*)
      (if(zerop(Org-requirements-count *org*))
	(warn "No requirements in ~S"(Org-name *org*))
	(let((count(length issues)))
	  (format t "~&~:[~D fail~:*~P in ~S~;Pass ~*~S~]~%"
		  (zerop count)
		  count
		  (Org-name *org*)))))
    :end
    (mapc #'print issues)))
