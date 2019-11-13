(in-package #:jingoh.generator)

;;;; GENERATOR
(declaim (ftype (function *
			  (values (function ()
					    (values null &optional))
				  &optional))
		asd-generator
		readme-generator
		method-extension-appender
		test-asd-generator
		cl-source-file-generator
		readme-updator
		))

;;; ASD
(defun asd-generator(system-name)
  (lambda()
    (let((*package*(find-package :asdf)))
      (format t "; vim: ft=lisp et~%~
	      (in-package :asdf)~%~
	      ~(~S~)~%"
	      `(asdf:defsystem ,system-name
			       :version "0.0.0"
			       :depends-on ()
			       :pathname "src/"
			       :components ((:file ,system-name)))))))

;;; README
(defun readme-generator(system-name)
  (let((system
	 (asdf:find-system system-name nil)))
    (lambda()
      (format t "# ~@:(~A~) ~A~%~
	      ## What is this?~%~
	      ~@[~A~]~%~
	      ### Current lisp world~2%~
	      ### Issues~2%~
	      ### Proposal~2%~
	      ## Usage~2%~
	      ## From developer~2%~
	      ### Product's goal~2%~
	      ### License~%~
	      ~@[~A~]~%~
	      ### Developed with~2%~
	      ### Tested with~2%~
	      ## Installation~2%"
	      system-name
	      (or (and system
		       (asdf:system-version system))
		  "0.0.0")
	      (and system
		   (asdf:system-description system))
	      (and system
		   (asdf:system-license system))
	      ))))

;;; METHOD-EXTENSION
(defun method-extension-appender(name)
  (lambda()
    (let((*package*(find-package :asdf)))
      (format t "~%;;; These forms below are added by JINGOH.GENERATOR.~{~%~A~%~(~S~)~}"
	      `(";; Ensure in ASDF for pretty printings."
		(in-package :asdf)
		,(format nil ";; Enable testing via (asdf:test-system ~S)."
			 name)
		(defmethod asdf:component-depends-on((asdf::o asdf:test-op)
						     (asdf::c (eql (asdf:find-system ,name))))
		  (append (call-next-method) '((asdf:test-op ,(Test-name name)))))
		";; Enable passing parameter for JINGOH:EXAMINER via ASDF:TEST-SYSTEM."
		(defmethod asdf:operate :around ((asdf::o asdf:test-op)
						 (asdf::c (eql (asdf:find-system ,name)))
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
		(let ((asdf::system
			(asdf:find-system "jingoh.documentizer" nil)))
		  (when (and asdf::system (not (uiop:featurep :clisp)))
		    (asdf:load-system asdf::system)
		    (defmethod asdf:perform :after
		      ((asdf::o asdf:load-op)
		       (asdf::c (eql (asdf:find-system ,name))))
		      (uiop:with-muffled-conditions(uiop:*uninteresting-conditions*)
			(uiop:symbol-call :jingoh.documentizer :import asdf::c))))))))))

;;; TEST-ASD
(defun test-asd-generator(system forms)
  (lambda()
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
					    ,@(mapcar #'EXAMINE-FORM forms))))))))

;;; CL-SOURCE-FILE
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

;;; README-UPDATOR
(defun readme-updator(system readme-lines)
  (lambda()
    (format t "# ~@:(~A~) ~:[0.0.0~;~:*~A~]~%"
	    (asdf:coerce-name system)
	    (asdf:component-version system))
    (do*((lines (cdr readme-lines)(cdr lines))
	 (line (car lines)(car lines)))
      ((endp lines)(force-output))
      (flet((SKIP-TO(prefix)
	      (setf lines
		    (nthcdr (position-if (lambda(line)
					   (uiop:string-prefix-p prefix line))
					 (cdr lines))
			    lines))))
	(cond
	  ((uiop:string-prefix-p "## What is this?" line)
	   (format t "~A~%~:[~;~:*~A~]~2%"
		   line
		   (asdf:system-description system))
	   (SKIP-TO "##"))
	  ((uiop:string-prefix-p "### License" line)
	   (format t "~A~%~:[TODO~;~:*~A~]~2%"
		   line
		   (asdf:system-license system))
	   (SKIP-TO "##"))
	  (t (write-line line)))))))

;;;; GENERATE
;;; SYMBOL
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
      (unless(probe-file path)
	(generate 'test-asd :system system :forms forms :path path))
      (dolist(form forms)
	(generate form :append t)))))

;;; INIT
#| Generate project skelton.
   project/---
            |---project.asd
            |---README.md
            |---src/---
                     |---project.lisp
|#

(defmethod generate((dispatcher (eql 'init))&key system pathname)
  (let*((system-name
	  (ensure-name system))
	(*default-pathname-defaults*
	  (uiop:subpathname (or (and pathname
				     (uiop:ensure-directory-pathname pathname))
				(local-project-directory))
			    (uiop:ensure-directory-pathname system-name))))
    (Output-to (Path-of system-name "asd")
	       (asd-generator system-name))
    (Output-to (Path-of "README" "md")
	       (readme-generator system-name))
    (Output-to (Path-of system-name "lisp" (uiop:subpathname *default-pathname-defaults*
							     "src/"))
	       (cl-source-file-generator system-name))
    (ql:register-local-projects)
    (generate (asdf:find-system system))))

(defun ensure-name(system)
  (let((name
	 (asdf:coerce-name system)))
    (if(not(find name (ql-dist:provided-systems t)
		 :key #'ql-dist:name
		 :test #'equal))
      name
      (restart-case (error "~S is already used in quicklisp." name)
	(continue()
	  :report "Use it anyway."
	  name)
	(rename(name)
	  :report "Specify new name."
	  :interactive (lambda()
			 (format *query-io* ">> ")
			 (force-output *query-io*)
			 (list(read-line *query-io*)))
	  (ensure-name name))))))

(defun local-project-directory()
  (let((directories ql:*local-project-directories*))
    (if(null(cdr directories))
      (car directories)
      (progn (loop :for d :in directories
		   :for i :upfrom 0
		   :do (format *query-io* "~%~3D: ~S"i d))
	     (nth (prompt-for:prompt-for
		    `(mod ,(length directories))
		    "~%Which directory do you use?~%Please type number >> ")
		  directories)))))

;;; SYSTEM
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
	  (Path-of (Test-name system)
		   "asd")))
    (mapc #'asdf:load-system(asdf:system-depends-on system))
    (asdf:load-system system :force t)
    ;; In order to generate asd for already existing project,
    ;; and avoid generate extension duplicatedly,
    ;; adding extension unless spec dir exists.
    (unless(probe-file test-asd-path)
      (add-method-extension system))
    (generate 'test-asd :system system :forms forms :path test-asd-path)
    (dolist(form forms)
      (generate form :append append)))
  #+quicklisp
  (ql:register-local-projects))

(defun add-method-extension (system)
  (let((system(asdf:find-system system)))
    (Output-to (asdf:system-source-file system)
	       (method-extension-appender (asdf:coerce-name system))
	       :if-exists :append)))

;;; TEST-ASD
(defmethod generate((dispatcher (eql 'test-asd)) &key system forms path)
  (ensure-directories-exist *default-pathname-defaults*)
  (Output-to path
	     (test-asd-generator system forms)))

;;; DEFPACKAGE-FORM
(defmethod generate((form list) &key append)
  (assert(typep form '(CONS (EQL DEFPACKAGE) T)))
  (labels((exports(form)
	    (loop :for elt :in form
		  :when (typep elt '(CONS(EQL :EXPORT)T))
		  :append (cdr elt))))
    (let*((path
	    (Path-of (string-downcase(second form))
		     "lisp"))
	  (*package*(find-package(second form)))
	  (existp
	    (probe-file path)))
      (Output-to path
		 (lambda()
		   (unless existp
		     (generate-header (second form)))
		   (dolist(symbol (exports form))
		     (symbol-generate symbol (second form))))
		 :if-exists (if (and existp
				     append)
			      :append
			      :error)))))

(defun generate-header(package-name)
  (let((spec-name(intern (format nil "~A.SPEC"package-name)
			 :keyword)))
    (format t "~(~S~)~%~
	    (in-package ~(~S~))~%~
	    (setup ~(~S~))~2%"
	    `(defpackage ,spec-name (:use :cl :jingoh ,package-name))
	    spec-name
	    (intern (string package-name):keyword)
	    )))

;;; :README
(defmethod generate((dispatcher (eql :readme))&key system)
  (declare(ignore dispatcher))
  (setf system (asdf:find-system system)) ; as canonicalize.
  (let*((readme-path
	  (Path-of "README" "md"(asdf:system-source-directory system)))
	(existp
	  (probe-file readme-path))
	(lines
	  (and existp
	       (uiop:read-file-lines readme-path))))
    (Output-to readme-path
	       (if (or existp
		       lines)
		 (readme-updator system lines)
		 (readme-generator (asdf:coerce-name system))))))
