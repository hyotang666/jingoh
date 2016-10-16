(in-package :jingoh.reporter)

(macrolet((!(form)
	    `(RESIGNAL-BIND((MISSING-ORG()'MISSING-ORG :API 'DEFAULT-REPORTER))
	       ,form)))

  (defun default-reporter(&rest names)
    #.(Doc :jingoh.reporter "doc/default-reporter.F.md")
    (flet((REPORT(org)
	    (if(zerop(Org-requirements-count org))
	      (warn "No requirements in ~S"(Org-name org))
	      (let((count(let((count 0))
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
    (let((*org*(! 1(Find-org org))))
      (Do-requirements(Requirement subject)
	(mapc #'print(Check requirement))))))
