(in-package #:jingoh.documentizer)

;;;; COMPILE
(defun compile(system &optional(*print-example* *print-example*))
  "Compile spec documentation to lisp file."
  (let*((system
	  (ensure-system system))
	(sys-dir
	  (asdf:system-source-directory system))
	(meta-datas
	  (Meta-datas<=system system sys-dir))
	(*default-pathname-defaults*
	  sys-dir)
	(*package*
	  #.(find-package :jingoh.documentizer)))
    (With-output-to((merge-pathnames "doc.lisp"))
      (dolist(meta meta-datas)
	(print `(in-package ,(meta-data-name meta)))
	(dolist(s(Meta-data-sections meta))
	  (map nil #'print (<documentations> s (Meta-data-name meta))))))))

(defun ensure-system(system)
  (restart-case(asdf:find-system system)
    (use-value(correct)
      :report "Specify correct system name."
      :interactive
      (lambda()
	(list (prompt-for:prompt-for T "~&>> "
				     :by (lambda(stream)
					   (asdf:find-system (read stream))))))
      correct)))

(define-condition no-doc-type(style-warning)
  ((name :initarg :name :reader name))
  (:report(lambda(condition stream)
	    (format stream "Ignore ~S due to no doc-type specified."
		    (name condition)))))

(defun no-doc-type(name)
  (warn 'no-doc-type :name name))

(defun <documentations>(section package)
  (loop :for name :in (Section-names section)
	:if (Section-doc-type section)
	:collect
	`(defmethod documentation ((s (eql (or (find-symbol ,(string name)
							    ,(string package))
					       (error "Not found symbol ~S in package ~S"
						      ,(string name)
						      ,(string package)))))
				   (type (eql ',(Section-doc-type section))))
	   (declare(ignore s type))
	   ,(princ-to-string section))
	:else :do (no-doc-type name)))

;;;; IMPORT
(defun import(system &optional(*print-example* *print-example*))
  "Import spec documentation to lisp image."
  (dolist(m (Meta-datas<=system (ensure-system system)))
    (dolist(s (Meta-data-sections m))
      (dolist(name (Section-names s))
	(let((doc-type
	       (Section-doc-type s)))
	  (if doc-type
	    (setf (documentation (find-symbol (symbol-name name)
					      (Meta-data-name m))
				 doc-type)
		  (princ-to-string s))
	    (no-doc-type name)))))))
