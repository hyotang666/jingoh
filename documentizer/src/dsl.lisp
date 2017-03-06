(in-package :jingoh.documentizer)

(defmacro with-doc-directory((pathname) &body body)
  `(WITH-OPEN-FILE(*STANDARD-OUTPUT* ,pathname
				     :DIRECTION :OUTPUT
				     :IF-DOES-NOT-EXIST :CREATE
				     :IF-EXISTS :SUPERSEDE)
     (cl-who:with-html-output(*standard-output* nil :prologue t)
       (:style #.colorize:*coloring-css*)
       (:html
	 (:body
	   (LET((3BMD-CODE-BLOCKS:*CODE-BLOCKS* T))
	     (PARSE-STRING-AND-PRINT-TO-STREAM (WITH-OUTPUT-TO-STRING(*STANDARD-OUTPUT*)
						 ,@body)
					       *STANDARD-OUTPUT*)))))))

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
	(sections(parse-spec pathname)))
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
							 :collect(section-names sec)))
		       ))))

(defun sieve (meta-data-sections)
  (loop :for sec :in meta-data-sections
	:if(single-p sec)
	:collect sec :into singles
	:if(common-p sec)
	:collect sec :into commons
	:finally(return (values singles commons))))
