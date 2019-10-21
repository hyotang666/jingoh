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
	  (Test-asd-path system)))
    (mapc #'asdf:load-system(asdf:system-depends-on system))
    (asdf:load-system system :force t)
    ;; In order to generate asd for already existing project,
    ;; and avoid generate extension duplicatedly,
    ;; adding extension unless spec dir exists.
    (unless(probe-file test-asd-path)
      (add-method-extension system))
    (generate-test-asd system forms test-asd-path)
    (dolist(form forms)
      (generate form :append append)))
  #+quicklisp
  (ql:register-local-projects))

(defun add-method-extension (system)
  (let((system(asdf:find-system system)))
    (uiop:with-output-file(*standard-output*
			    (asdf:system-source-file system)
			    :if-exists
			    :append)
      (%add-method-extension (asdf:coerce-name system)))))

;; Splitted for easy debug/test.
(defun %add-method-extension(name)
  (let((*package*(find-package :asdf)))
    (format t "~%;;; These forms below are added by JINGOH.GENERATOR.~{~%~A~%~(~S~)~}"
	    `(";; Ensure in ASDF for pretty printings."
	      (in-package :asdf)
	      ";; Enable testing via (asdf:test-system :system-name)."
	      (defmethod asdf:component-depends-on((asdf::o asdf:test-op)(asdf::c (eql (asdf:find-system ,name))))
		(append (call-next-method) '((asdf:test-op ,(Test-name name)))))
	      ";; Enable passing parameter for JINGOH:EXAMINER via ASDF:TEST-SYSTEM."
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
	      ";; Enable importing spec documentations."
	      (let((asdf::system
		     (asdf:find-system "jingoh.documentizer" nil)))
		(when(and asdf::system
			  (not (uiop:featurep :clisp)))
		  (asdf:load-system asdf::system)
		  (defmethod asdf:operate :around ((asdf::o asdf:load-op)
						   (asdf::c (eql (asdf:find-system ,name)))
						   &key)
		    (let*((asdf::seen nil)
			  (*default-pathname-defaults*
			    (merge-pathnames "spec/"
					     (asdf:system-source-directory asdf::c)))
			  (*macroexpand-hook*
			    (let((asdf::outer-hook *macroexpand-hook*))
			      (lambda(asdf::expander asdf::form asdf::env)
				(if(not(typep asdf::form '(cons (eql defpackage)*)))
				  (funcall asdf::outer-hook asdf::expander
					   asdf::form asdf::env)
				  (if(find (cadr asdf::form)asdf::seen
					   :test #'string=)
				    (funcall asdf::outer-hook asdf::expander
					     asdf::form asdf::env)
				    (progn (push (cadr asdf::form) asdf::seen)
					   `(progn ,asdf::form
						   ,@(uiop:symbol-call
						       :JINGOH.DOCUMENTIZER
						       :IMPORTER
						       asdf::form)))))))))
		      (call-next-method)))))))))

(defun generate-test-asd(system forms test-asd-path)
  (ensure-directories-exist *default-pathname-defaults*)
  (uiop:with-output-file(*standard-output* test-asd-path :if-exists :supersede)
    (%generate-test-asd system forms)))

;; Splitted for easy debug/test.
(defun %generate-test-asd(system forms)
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
	      `(asdf:defsystem ,(Test-name system)
			       :version "0.0.0"
			       :depends-on (:jingoh ,(asdf:coerce-name system))
			       :components ,(mapcar #'COMPONENT forms)
			       :perform (asdf:test-op(asdf::o asdf::c)
					  (declare(special asdf::args))
					  ,@(mapcar #'EXAMINE-FORM forms)))))))
