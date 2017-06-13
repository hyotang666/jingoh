(in-package :jingoh.documentizer)

(defun about-package(meta-data)
  (With-doc-directory((merge-pathnames(format nil "P_~A.html"(Meta-data-name meta-data))
))
    (%about-package meta-data)))

(defun %about-package (meta-data)
  (format t "# ~A~%~@[## ~:*~A Concepts~%~A~%~]## ~A Dictionary~2%~{* ~A~&~}"
	  (Meta-data-name meta-data)
	  (Meta-data-doc meta-data)
	  (Meta-data-name meta-data)
	  (labels((REC(exports &optional acc)
		    (if(endp exports)
		      (nreverse acc)
		      (BODY (car exports)(cdr exports)acc)))
		  (FIND-IN-SEC(symbol sections)
		    (find-if (lambda(section)
			       (find symbol (Section-names section)
				     :test #'string=))
			     sections))
		  (MARKUP(symbol sec)
		    (format nil "[~A](~A)"
			    (Escape-* symbol)
			    (Section-path sec)))
		  (BODY(symbol rest acc)
		    (let((sec(FIND-IN-SEC symbol
					  (Meta-data-singles meta-data))))
		      (if sec
			(REC rest (push (MARKUP symbol sec) acc))
			(let((sec(FIND-IN-SEC symbol
					      (Meta-data-commons meta-data))))
			  (if sec
			    (REC rest (push (MARKUP symbol sec) acc))
			    (REC rest (push (Escape-* symbol) acc))))))))
	    (REC (Meta-data-exports meta-data)))))
