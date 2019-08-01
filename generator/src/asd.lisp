(in-package :jingoh.generator)

(defun generate-asd(system forms test-asd-path)
  (ensure-directories-exist *default-pathname-defaults*)
  (uiop:with-output-file(*standard-output* test-asd-path :if-exists :supersede)
    (%generate-asd system forms)))

(defun %generate-asd(system forms)
  (labels((COMPONENT(form)
	    `(:file ,(string-downcase(second form))))
	  (EXAMINE-FORM(form)
	    `(apply #'uiop:symbol-call :jingoh :examine ,(PACKAGE-KEY (second form))
		    asdf::args))
	  (PACKAGE-KEY(package-name)
	    (intern (string package-name) :keyword))
	  )
    (let((*package* (find-package :asdf)))
      (format t "; vim: ft=lisp et~%~
	      (in-package :asdf)~%~
	      ~(~S~)"
	      `(asdf:defsystem ,(intern (format nil "~:@(~A~).TEST"
						(asdf:coerce-name system))
					:keyword)
			       :version "0.0.0"
			       :depends-on (:jingoh ,(asdf:coerce-name system))
			       :components ,(mapcar #'COMPONENT forms)
			       :perform (asdf:test-op(asdf::o asdf::c)
					  (declare(special asdf::args))
					  ,@(mapcar #'EXAMINE-FORM forms)))))))

