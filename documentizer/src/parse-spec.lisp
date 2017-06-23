(defpackage :jingoh.documentizer.parse-spec(:use :cl :read-as-string)
  (:import-from :jingoh.documentizer.utility
		#:Target-path
		#:Replace-invalid-chars)
  (:import-from :jingoh.documentizer.sections
		#:Make-common
		#:Make-single
		#:Single-p
		)
  (:export
    ; main api
    #:parse-spec
    ))
(in-package :jingoh.documentizer.parse-spec)

(defun parse-spec(pathname)
  (engroup(sectionize(enlist pathname))))

(defun enlist(pathname)
  (with-open-file(s pathname)
    (loop :for exp = (Read-as-string s nil nil)
	  :while exp
	  :collect exp)))

(defun sectionize(list)
  (labels((REC(list &optional temp acc)
	    (if(endp list)
	      (nreverse (push (nreverse temp) acc))
	      (BODY (car list)(cdr list)temp acc)))
	  (BODY(exp-string rest temp acc)
	    (case (char exp-string 0)
	      (#\( (let*((*package* (find-package :jingoh.documentizer.parse-spec))
			 (sexp(read-from-string exp-string)))
		     (case (car sexp)
		       ((defpackage in-package setup)
			(REC rest temp acc)) ; discard
		       ((requirements-about common-requirements-about)
			(REC rest (list (getf (cddr sexp):as)
					(second sexp))
			     (when temp
			       (push (nreverse temp) acc)
			       acc)))
		       (t (REC rest (push exp-string temp) acc)))))
	      (T (REC rest (push exp-string temp)acc)))))
    (REC list)))

(defun engroup (sections)
  (flet((PARSE(sections)
	  (loop :for sec :in sections
		:when (consp (car sec))
		:collect sec :into commons
		:when (symbolp (car sec))
		:collect sec :into singles
		:finally(return (values singles commons)))))
    (multiple-value-bind(singles commons)(PARSE sections)
      (labels((REC(list &optional acc)
		(if(endp list)
		  (DO-RETURN commons acc)
		  (BODY (car list)(cdr list) acc)))
	      (DO-RETURN(commons acc)
		(nconc (loop :for c :in commons
			     :collect (Make-common :body (cddr c)
						   :names (car c)
						   :alias (cadr c)
						   :path (Target-path(symbol-name(gensym "C_")))))
		       (loop :for s :in acc
			     :collect (Make-single :body (cddr s)
						   :name (car s)
						   :path (Target-path(format nil "S_~A"(Replace-invalid-chars (car s))))))))
	      (BODY(sec rest acc)
		(let((pos (SINGLE-IN-COMMON-P sec)))
		  (if pos
		    (progn (INCLUDE-SEC-INTO-COMMONS pos sec)
			   (REC rest acc))
		    (REC rest (push sec acc)))))
	      (INCLUDE-SEC-INTO-COMMONS(pos sec)
		(setf (nth pos commons)
		      (nconc (nth pos commons)
			     (rplaca sec (ENCHAPTER(car sec))))))
	      (SINGLE-IN-COMMON-P(sec)
		(position-if (lambda(com)
			       (find (car sec)(car com)
				     :test #'string=))
			     commons))
	      (ENCHAPTER(title)
		(format nil "#| ~A |#"title))
	      )
	(REC singles)))))
