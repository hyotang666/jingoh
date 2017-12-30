(in-package :jingoh.generator)

(defmethod generate ((system asdf:system) &key append)
  (let(forms)
    (labels((LOAD-DEPENDENCIES(system)
	      (mapc #'asdf:load-system(asdf:system-depends-on system)))
	    (HOOK(expander form env)
	      (when(DEFPACKAGEP form)
		(push form forms))
	      (funcall expander form env))
	    (DEFPACKAGEP(form)
	      (typep form '(CONS(EQL DEFPACKAGE)T)))
	    )
      (LOAD-DEPENDENCIES system)
      (let((*macroexpand-hook* #'HOOK))
	(asdf:load-system system :force t))
      (let*((*default-pathname-defaults*(spec-directory system))
	    (test-asd-path(test-asd-path system)))
	(add-perform system test-asd-path)
	(generate-asd system forms test-asd-path)
	(dolist(form forms)
	  (generate form :append append)))))
  #+quicklisp
  (ql:register-local-projects))

(defun test-asd-path(system)
  (make-pathname :name (concatenate 'string (asdf:coerce-name system) ".test")
		 :type "asd"
		 :defaults *default-pathname-defaults*))

(defun add-perform (system test-asd-path)
  (unless(probe-file test-asd-path)
    (let((directory(asdf:system-source-file system)))
      (uiop:with-output-file(*standard-output* directory :if-exists :append)
	(%add-perform (asdf:coerce-name system))))))

(defun %add-perform(name)
  (let((*package*(find-package :asdf)))
    (format t "~%;; These two methods below are added by JINGOH.GENERATOR.~%~(~S~)~%~(~S~)"
	    `(defmethod asdf:perform((asdf::o asdf:test-op)(asdf::c (eql (asdf:find-system ,name))))
	       (asdf:test-system ,(intern(format nil "~:@(~A~).TEST"name)
			       :keyword)))
	    `(defmethod asdf:operate :around ((asdf::o asdf:test-op)(asdf::c (eql (asdf:find-system ,name)))
					      &rest asdf::keys)
	       (flet((asdf::jingoh.args(asdf::keys)
		       (loop :for (asdf::key asdf::value) :on asdf::keys :by #'cddr
			     :when (find asdf::key '(:on-fails :subject :vivid) :test #'eq)
			     :collect asdf::key :and :collect asdf::value
			     :else :when (eq :jingoh.verbose asdf::key)
			     :collect :verbose :and :collect asdf::value)))
		 (let((asdf::args(asdf::jingoh.args asdf::keys)))
		   (declare(special asdf::args))
		   (call-next-method)))))))

(defun spec-directory(system)
  (uiop:subpathname (asdf:system-source-directory (asdf:find-system system))
		    "spec/"))

