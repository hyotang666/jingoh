(in-package :jingoh.generator)

(defmethod generate((form list) &key append)
  (assert(typep form '(CONS (EQL DEFPACKAGE) T)))
  (macrolet((expand(existp)
	      `(WITH-OPEN-FILE(*STANDARD-OUTPUT* PATH
				:DIRECTION :OUTPUT
				,@(if existp
				    `(:IF-EXISTS :APPEND)
				    `(:IF-DOES-NOT-EXIST :CREATE)))
		 ,@(unless existp
		     `((GENERATE-HEADER(SECOND FORM))))
		 (DOLIST(SYMBOL (EXPORTS FORM))
		   (SYMBOL-GENERATE SYMBOL (SECOND FORM))))))
    (labels((exports(form)
	      (loop :for elt :in form
		    :when (typep elt '(CONS(EQL :EXPORT)T))
		    :append (cdr elt))))
      (let((path(format nil "~A~(~A~).lisp"
			*default-pathname-defaults*
			(second form)))
	   (*package*(find-package(second form))))
	(if (probe-file path)
	  (when append
	    (expand t))
	  (expand nil))))))

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
