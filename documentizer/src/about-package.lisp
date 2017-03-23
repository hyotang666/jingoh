(in-package :jingoh.documentizer)

(defun about-package(meta-data)
  (with-doc-directory((merge-pathnames(format nil "P_~A.html"(meta-data-name meta-data))
))
    (%about-package meta-data)))

(defun %about-package (meta-data)
  (format t "# ~A~%~@[## ~:*~A Concepts~%~A~%~]## ~A Dictionary~2%~{* ~A~&~}"
	  (meta-data-name meta-data)
	  (meta-data-doc meta-data)
	  (meta-data-name meta-data)
	  (labels((REC(exports &optional acc)
		    (if(endp exports)
		      (nreverse acc)
		      (BODY (car exports)(cdr exports)acc)))
		  (FIND-IN-SEC(symbol sections)
		    (find-if (lambda(section)
			       (find symbol (section-names section)
				     :test #'string=))
			     sections))
		  (MARKUP(symbol sec)
		    (format nil "[~A](~A)"
			    (escape-* symbol)
			    (section-html sec)))
		  (BODY(symbol rest acc)
		    (let((sec(FIND-IN-SEC symbol
					  (meta-data-singles meta-data))))
		      (if sec
			(REC rest (push (MARKUP symbol sec) acc))
			(let((sec(FIND-IN-SEC symbol
					      (meta-data-commons meta-data))))
			  (if sec
			    (REC rest (push (MARKUP symbol sec) acc))
			    (REC rest (push (escape-* symbol) acc))))))))
	    (REC (meta-data-exports meta-data)))))
