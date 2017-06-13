(in-package :jingoh.documentizer)

(defun symbol-index(meta-datas system)
  (With-doc-directory((merge-pathnames(Target-path "symbols")))
    (%symbol-index meta-datas system)))

(defun %symbol-index (meta-datas system)
  (labels((LINKS(chars &optional(code #.(char-code #\A)) have-non-alph-p acc)
	    (if(not(<= code #.(char-code #\Z)))
	      (nreconc acc (or have-non-alph-p '("Non-Alphabetic")))
	      (let((char(car chars)))
		(if(null char)
		  (LINKS chars (1+ code) have-non-alph-p
			 (push (princ-to-string (code-char code))acc))
		  (if(not(alpha-char-p char))
		    (LINKS (cdr chars) code
			   (pushnew (format nil "[Non-Alphabetic](~A)" *x-non-alph-namestring*)
				    have-non-alph-p :test #'string=)
			   acc)
		    (if(= code (char-code char))
		      (LINKS (cdr chars) (1+ code) have-non-alph-p
			     (push (format nil "[~A](~A)"
					   char
					   (X-alph-pathname char))
				   acc))
		      (LINKS chars (1+ code) have-non-alph-p
			     (push (princ-to-string (code-char code))acc)))))))))
    (let*((symbols(apply #'append (mapcar #'Meta-data-specifieds meta-datas)))
	  (index-chars(Index-chars symbols)))
      (format t "# Alphabetical Symbol Index~2%There ~:[is~;are~] ~D symbol~:*~P by ~A.~2%~{~A~^ | ~}"
	      (not(= 1(length symbols)))
	      (length symbols)
	      (asdf:coerce-name system)
	      (LINKS index-chars)))))
