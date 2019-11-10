(in-package :jingoh.documentizer)

(define-condition missing-spec-file(style-warning)
  ((path :initarg :path :reader path))
  (:report (lambda(condition stream)
	     (format stream "Missing spec file. ~S"
		     (path condition)))))

(defun missing-spec-file(pathname)
  (warn 'missing-spec-file :path pathname))

(defstruct(meta-data (:constructor %make-meta-data)
		     (:copier nil)
		     (:predicate nil))
  (name nil :type symbol) ; pacakge-name
  (exports nil :type list) ; exported symbol extract from defpackage form.
  (doc nil :type (or null string)) ; documentation extract from defpacakge form.
  (sections nil :type list) ; all sections
  (singles nil :type list) ; sections which specified requirement.
  (commons nil :type list) ; sections which specified common requirement.
  (specifieds nil :type list) ; symbols which specified.
  )

(defun make-meta-data(form)
  (let*((pathname(make-pathname :name (string-downcase (string(second form)))
				:type "lisp"
				:defaults *default-pathname-defaults*))
	(sections(Parse-spec pathname)))
    (multiple-value-bind(singles commons)(sieve sections)
      (%make-meta-data :name (second form)
		       :exports (loop :for option :in (cddr form)
				      :when (eq :export (car option))
				      :append (cdr option))
		       :doc (let((option(assoc :documentation (cddr form))))
			      (when option
				(second option)))
		       :sections sections
		       :singles singles
		       :commons commons
		       :specifieds (apply #'append (loop :for sec :in sections
							 :collect(Section-names sec)))
		       ))))

(defun sieve (meta-data-sections)
  (loop :for sec :in meta-data-sections
	:if(Single-p sec)
	:collect sec :into singles
	:if(Common-p sec)
	:collect sec :into commons
	:finally(return (values singles commons))))

(defun meta-datas<=system(system
			   &optional
			   (sys-dir(asdf:system-source-directory system)))
  (let((spec-dir(merge-pathnames "spec/" sys-dir)))
    (when(not(uiop:directory-exists-p spec-dir))
      (return-from meta-datas<=system (missing-spec-file spec-dir)))
    (mapc #'asdf:load-system (asdf:system-depends-on system))
    (let(meta-datas)
      (let((*macroexpand-hook*
	     (let((outer-hook *macroexpand-hook*))
	       (lambda(expander form env)
		 (when(typep form '(cons (eql defpackage)T))
		   (push form meta-datas))
		 (funcall outer-hook expander form env))))
	   (asdf::*asdf-session* nil))
	(asdf:load-system system :force t))
      (let((*default-pathname-defaults* spec-dir))
	(mapcar #'make-meta-data meta-datas)))))
