(in-package :jingoh.documentizer)

(defun table(meta-datas)
  (labels((LINE(sec)
	    (loop :for name :in (section-names sec)
		  :collect (cons name
				 (format nil "[~A](~A)~%"
					 (escape-* name)
					 (section-html sec))))))
    (let*((pairs(loop :for sec :in (apply #'append (mapcar #'meta-data-sections meta-datas))
		      :nconc (LINE sec) :into result
		      :finally (return (sort result #'string< :key #'car))))
	  (index-chars(index-chars(apply #'append (mapcar #'meta-data-specifieds meta-datas)))))
      (dolist(char index-chars)
	(with-doc-directory((if(alpha-char-p char)
			      (x-alph-pathname char)
			      +x-non-alph-namestring+))
	  (do((pair(car pairs)(car pairs)))
	    ((or (null pairs)
		 (not(char= char (first-char(car pair))))))
	    (format t "* ~A~&"(cdr pair))
	    (pop pairs)))))))

