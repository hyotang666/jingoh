(in-package :jingoh.generator)

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
      (uiop:with-output-file(*standard-output* path
					       :if-exists
					       (if (and existp
							append)
						 :append
						 :error))
	(unless existp
	  (generate-header (second form)))
	(dolist(symbol (exports form))
	  (symbol-generate symbol (second form)))))))

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
