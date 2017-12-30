(in-package :jingoh.generator)

(defun generate-asd(system forms test-asd-path)
  (ensure-directories-exist *default-pathname-defaults*)
  (uiop:with-output-file(*standard-output* test-asd-path :if-exists :supersede)
    (%generate-asd system forms)))

(defun %generate-asd(system forms)
  (labels((COMPONENT(form)
	    `(:file ,(string-downcase(second form))))
	  (EXAMINE-FORM(form)
	    `(uiop:symbol-call :jingoh :examine ,(PACKAGE-KEY (second form))
			       :subject asdf::subject
			       :verbose asdf::verbose
			       :on-fails asdf::on-fails
			       :vivid asdf::vivid))
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
			       :depends-on (:jingoh ,(asdf:coerce-name system))
			       :components ,(mapcar #'COMPONENT forms)
			       :perform (asdf:test-op(asdf::o asdf::c)
					  (declare(special asdf::subject asdf::verbose asdf::on-fails asdf::vivid))
					  ,@(mapcar #'EXAMINE-FORM forms)))))))

