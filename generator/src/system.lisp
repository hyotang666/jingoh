(in-package :jingoh.generator)

(defmethod generate ((system asdf:system) &key append)
  (let*((forms)
	(*macroexpand-hook*
	  (let((outer-hook
		 *macroexpand-hook*))
	    (lambda(expander form env)
	      (when(typep form '(cons (eql defpackage)*))
		(push form forms))
	      (funcall outer-hook expander form env))))
	(*default-pathname-defaults*
	  (Spec-directory system))
	(test-asd-path
	  (test-asd-path system)))
    (mapc #'asdf:load-system(asdf:system-depends-on system))
    (asdf:load-system system :force t)
    (add-method-extension system test-asd-path)
    (generate-asd system forms test-asd-path)
    (dolist(form forms)
      (generate form :append append)))
  #+quicklisp
  (ql:register-local-projects))

(defun test-asd-path(system)
  (make-pathname :name (test-name (asdf:coerce-name system))
		 :type "asd"
		 :defaults *default-pathname-defaults*))

(defun test-name(name)
  (concatenate 'string name ".test"))

(defun add-method-extension (system test-asd-path)
  (unless(probe-file test-asd-path)
    (let((directory(asdf:system-source-file system)))
      (uiop:with-output-file(*standard-output* directory :if-exists :append)
	(%add-method-extension (asdf:coerce-name system))))))

(defun %add-method-extension(name)
  (let((*package*(find-package :asdf)))
    (format t "~%;; These forms below are added by JINGOH.GENERATOR.~{~%~(~S~)~}"
	    `((in-package :asdf)
	      (defmethod asdf:component-depends-on((asdf::o asdf:test-op)(asdf::c (eql (asdf:find-system ,name))))
		(append (call-next-method) '((asdf:test-op ,(test-name name)))))
	      (defmethod asdf:operate :around ((asdf::o asdf:test-op)(asdf::c (eql (asdf:find-system ,name)))
					       &rest asdf::keys
					       &key ((:compile-print *compile-print*))
					       ((:compile-verbose *compile-verbose*))
					       &allow-other-keys)
		(flet((asdf::jingoh.args(asdf::keys)
			(loop :for (asdf::key asdf::value) :on asdf::keys :by #'cddr
			      :when (find asdf::key '(:on-fails :subject :vivid) :test #'eq)
			      :collect asdf::key :and :collect asdf::value
			      :else :when (eq :jingoh.verbose asdf::key)
			      :collect :verbose :and :collect asdf::value)))
		  (let((asdf::args(asdf::jingoh.args asdf::keys)))
		    (declare(special asdf::args))
		    (call-next-method))))
	      (let((asdf::system
		     (asdf:find-system "jingoh.documentizer" nil)))
		(when asdf::system
		  (asdf:load-system asdf::system)
		  (defmethod asdf:operate :around ((asdf::o asdf:load-op)
						   (asdf::c (eql (asdf:find-system ,name)))
						   &key)
		    (let*((asdf::forms nil)
			  (*macroexpand-hook*
			    (let((asdf::outer-hook *macroexpand-hook*))
			      (lambda(asdf::expander asdf::form asdf::env)
				(when(typep asdf::form '(cons (eql defpackage)*))
				  (push asdf::form asdf::forms))
				(funcall asdf::outer-hook asdf::expander asdf::form asdf::env))))
			  (*default-pathname-defaults*
			    (merge-pathnames "spec/"
					     (asdf:system-source-directory asdf::c))))
		      (multiple-value-prog1(call-next-method)
			(mapc (find-symbol (string :importer)
					   :jingoh.documentizer)
			      asdf::forms))))))))))
