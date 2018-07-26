(in-package :jingoh.generator)

(defmethod generate ((symbol symbol) &key system init pathname)
  (if(keywordp symbol)
    (if init
      (generate 'init :system symbol :pathname pathname)
      (generate(asdf:find-system symbol)))
    (let*((*package* (symbol-package symbol))
	  (package-name(package-name *package*))
	  (system(asdf:find-system (or system (string-downcase package-name))))
	  (*default-pathname-defaults*(spec-directory system))
	  (forms `((defpackage ,package-name
		     (:export ,symbol))))
	  (path(test-asd-path system)))
      (macrolet((expand(existsp)
		  `(PROGN ,@(unless existsp
			      `((GENERATE-ASD SYSTEM FORMS PATH)))
			  (DOLIST(FORM FORMS)
			    (GENERATE FORM :APPEND T)))))
	(if(probe-file path)
	  (expand T)
	  (expand nil))))))

