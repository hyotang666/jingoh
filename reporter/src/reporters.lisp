(in-package :jingoh.reporter)

(macrolet((!(form)
	    `(RESIGNAL-BIND((MISSING-ORG()'MISSING-ORG :API 'DEFAULT-REPORTER))
	       ,form)))

  (defun default-reporter(&rest names)
    #.(doc :jingoh.reporter "doc/default-reporter.F.md")
    (flet((report(org)
	    (if(zerop(org-requirements-count org))
	      (warn "No requirements in ~S"(org-name org))
	      (let((count(let((count 0))
			   (!(do-requirements(requirement nil org count)
			       (incf count (length(check requirement))))))))
		(format t "~&~:[~D fail~:*~P in ~S~;Pass ~*~S~].~%"
			(zerop count)
			count
			(org-name org))))))
      (if(null names)
	(report *org*)
	(dolist(name names)
	  (report(find-org name)))))))

(defparameter *reporter* #'default-reporter
  #.(doc :jingoh.reporter "doc/AreporterA.V.md"))

(macrolet((!(n form)
	    `(RESIGNAL-BIND((MISSING-ORG(C)'MISSING-ORG
			      :API ',(nth n '(REPORT DETAIL))))
	       ,form)))

  (defun report(&rest args)
    #.(doc :jingoh.reporter "doc/report.F.md")
    (! 0(apply *reporter* args)))

  (defun detail(&key subject(org *org*))
    #.(doc :jingoh.reporter "doc/detail.F.md")
    (let((*org*(! 1(find-org org))))
      (do-requirements(requirement subject)
	(mapc #'print(check requirement))))))
