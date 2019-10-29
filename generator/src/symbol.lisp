(in-package :jingoh.generator)

(defmethod generate ((symbol symbol) &key system init pathname append)
  (if(keywordp symbol)
    (if init
      (generate 'init :system symbol :pathname pathname)
      (generate(asdf:find-system symbol):append append))
    (let*((*package* (symbol-package symbol))
	  (package-name(package-name *package*))
	  (system(asdf:find-system (or system (string-downcase package-name))))
	  (*default-pathname-defaults*(Spec-directory system))
	  (forms `((defpackage ,package-name
		     (:export ,symbol))))
	  (path
	    (Path-of (Test-name system)
		     "asd")))
      (macrolet((expand(existsp)
		  `(PROGN ,@(unless existsp
			      `((GENERATE-TEST-ASD SYSTEM FORMS PATH)))
			  (DOLIST(FORM FORMS)
			    (GENERATE FORM :APPEND T)))))
	(if(probe-file path)
	  (expand T)
	  (expand nil))))))

