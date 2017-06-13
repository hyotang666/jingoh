(in-package :jingoh.documentizer)

(defun table(meta-datas &optional(callback #'table-printer))
  (labels((LINE(sec)
	    (loop :for name :in (Section-names sec)
		  :collect (cons name
				 (format nil "[~A](~A)~%"
					 (Escape-* name)
					 (Section-path sec))))))
    (let*((pairs(loop :for sec :in (apply #'append (mapcar #'Meta-data-sections meta-datas))
		      :nconc (LINE sec) :into result
		      :finally (return (sort result #'string< :key #'car))))
	  (index-chars(Index-chars(apply #'append (mapcar #'Meta-data-specifieds meta-datas)))))
      (labels((REC(chars pairs)
		(unless(endp chars)
		  (apply #'REC (funcall callback chars pairs)))))
	(REC index-chars pairs)))))

(defun table-callback(chars pairs)
  (let((char (car chars))
       return)
    (With-doc-directory((if(alpha-char-p char)
			  (X-alph-pathname char)
			  *X-non-alph-namestring*))
      (setf return(table-printer chars pairs)))
    return))

(defun table-printer(chars pairs)
  (if(endp pairs)
    (list nil nil)
    (if(char= (car chars) (First-char(caar pairs)))
      (progn #1=(format t "* ~A~&"(cdar pairs))
	     (table-printer chars (cdr pairs))) ; cdring pairs.
      (if(alpha-char-p(car chars))
	#0=(list (cdr chars)pairs) ; cdring chars.
	(if(alpha-char-p(First-char(caar pairs)))
	  #0# ; cdring chars.
	  (progn #1#
		 (table-printer (cdr chars)(cdr pairs)))))))) ; cdring pairs and chars.
