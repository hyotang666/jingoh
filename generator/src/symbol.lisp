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
      (when(probe-file path)
	(Generate-test-asd system forms path))
      (dolist(form forms)
	(generate form :append t)))))

