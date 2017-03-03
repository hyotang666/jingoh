(in-package :jingoh.generator)

(defun generate-asd(system forms)
  (ensure-directories-exist *default-pathname-defaults*)
  (with-open-file(*standard-output* (format nil "~A~A.test.asd"
					    *default-pathname-defaults*
					    (asdf:coerce-name system))
				    :direction :output
				    :if-exists :supersede
				    :if-does-not-exist :create)
    (%generate-asd system forms)))

(defun %generate-asd(system forms)
  (labels((component(form)
	    `(:file ,(string-downcase(second form)))))
    (let((*package* (find-package :asdf)))
      (format t "; vim: ft=lisp et~%~
	      (in-package :asdf)~%~
	      ~(~S~)"
	      `(asdf:defsystem ,(intern (format nil "~:@(~A~).TEST"
						(asdf:coerce-name system))
					:keyword)
			  :depends-on (:jingoh ,(asdf:coerce-name system))
			  :components ,(mapcar #'component forms)
			  :perform (asdf:test-op(asdf::o asdf::c)
				     (uiop:symbol-call :jingoh :examine)))))))

