(in-package :jingoh.generator)

(defmethod generate((dispatcher (eql 'init))&key system pathname)
  (let*((system-name(asdf:coerce-name system))
	(*default-pathname-defaults*(uiop:subpathname (or (and pathname
							       (uiop:ensure-directory-pathname pathname))
							  (local-project-directory))
						      (uiop:ensure-directory-pathname system-name))))
    (flet((output-to(path thunk)
	    (uiop:with-output-file(*standard-output*(ensure-directories-exist path)
				    :if-exists :supersede)
	      (funcall thunk)))
	  (path-of(name type)
	    (make-pathname :name name
			   :type type
			   :defaults *default-pathname-defaults*)))
      (output-to (path-of system-name "asd")
		 (asd-generator system-name))
      (output-to (path-of "README" "md")
		 (readme-generator system-name))
      (output-to (let((*default-pathname-defaults*(uiop:subpathname *default-pathname-defaults*
								    "src/")))
		   (path-of system-name "lisp"))
		 (cl-source-file-generator system-name)))
    (ql:register-local-projects)
    (generate(asdf:find-system system-name))))

(defun local-project-directory()
  (let((directories ql:*local-project-directories*))
    (if(null(cdr directories))
      (car directories)
      (progn (loop :for d :in directories
		   :for i :upfrom 0
		   :do (format *query-io* "~%~3D: ~S"i d))
	     (nth (prompt-for:prompt-for `(mod ,(length directories)) "~%Which directory do you use?~%Please type number >> ")
		  directories)))))

(defun asd-generator(system-name)
  (lambda()
    (let((*package*(find-package :asdf)))
      (format t "; vim: ft=lisp et~%~
	      (in-package :asdf)~%~
	      ~(~S~)~%"
	      `(asdf:defsystem ,system-name
			       :depends-on ()
			       :pathname "src/"
			       :components ((:file ,system-name)))))))

(defun cl-source-file-generator(system-name)
  (let((package-name(intern (string-upcase system-name)
			    :keyword)))
    (lambda()
      (format t "~(~S~)~%~(~S~)~%~(~S~)~2%"
	      '(in-package :cl-user)
	      `(defpackage ,package-name
		 (:use :cl)
		 (:export))
	      `(in-package ,package-name)))))

(defun readme-generator(system-name)
  (lambda()
    (format t "# ~A 0.0.0~%~
	    ## What is this?~2%~
	    ### Current lisp world~2%~
	    ### Issues~2%~
	    ### Proposal~2%~
	    ## Usage~2%~
	    ## From developer~2%~
	    ### Product's goal~2%~
	    ### License~2%~
	    ### Developed with~2%~
	    ### Tested with~2%~
	    ## Installation~2%"
	    system-name)))
