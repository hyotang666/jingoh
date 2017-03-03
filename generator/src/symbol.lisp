(in-package :jingoh.generator)

(defmethod generate ((symbol symbol) &key system)
  (if(keywordp symbol)
    (generate(asdf:find-system symbol))
    (let*((*package* (symbol-package symbol))
	  (package-name(package-name *package*))
	  (system(asdf:find-system (or system (string-downcase package-name))))
	  (*default-pathname-defaults*(spec-directory system))
	  (forms `((defpackage ,package-name
		     (:export ,symbol))))
	  (path(format nil "~A.test.asd"
		       *default-pathname-defaults*)))
      (macrolet((expand(existsp)
		  `(PROGN ,@(unless existsp
			      `((GENERATE-ASD SYSTEM FORMS)))
			  (MAP NIL #'GENERATE FORMS))))
	(if(probe-file path)
	  (expand T)
	  (expand nil))))))

