(in-package :jingoh.generator)

(defmethod generate ((system asdf:system) &key)
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
      (let((*default-pathname-defaults*(spec-directory system)))
	(add-perform system)
	(generate-asd system forms)
	(map nil #'generate forms)))))

(defun add-perform (system)
  (let((directory(asdf:system-source-file system)))
    (with-open-file(*standard-output* directory :direction :output
		      :if-exists :append)
      (%add-perform (asdf:coerce-name system)))))

(defun %add-perform(name)
  (let((*package*(find-package :asdf)))
    (format t "~&~(~S~)"
	    `(defmethod asdf:perform((asdf::o asdf:test-op)(asdf::c (eql (asdf:find-system ,name))))
	       (asdf:test-system ,(intern(format nil "~:@(~A~).TEST"name)
			       :keyword))))))

(defun spec-directory(system)
  (uiop:subpathname (asdf:system-source-directory (asdf:find-system system))
		    "spec/"))

