(defpackage :jingoh.documentizer.parse-spec(:use :cl :read-as-string)
  (:import-from :jingoh.documentizer.utility
		#:Target-path
		#:Replace-invalid-chars)
  (:export
    ; main api
    #:parse-spec
    ; slot readers
    #:section-body
    #:section-path
    #:section-names
    ; subtype predicates
    #:single-p
    #:common-p
    ))
(in-package :jingoh.documentizer.parse-spec)

(defun parse-spec(pathname)
  (engroup(sectionize(enlist pathname))))

(defstruct section body path names)
(defstruct(single (:include section)
		  (:constructor make-single (&key body path name
						  &aux(names (list name))))))
(defstruct(common (:include section))
  alias)

(defmethod print-object ((obj single)*standard-output*)
  (if *print-escape*
    (print-unreadable-object(obj *standard-output* :type nil :identity nil)
      (prin1 (car(section-names obj))))
    (call-next-method)))

(defmethod print-object((obj common)*standard-output*)
  (if *print-escape*
    (print-unreadable-object(obj *standard-output* :type nil :identity nil)
      (prin1 (common-alias obj)))
    (progn (format t "# ~{~A^, ~}~%"(common-names obj))
	   (call-next-method))))

(defmethod print-object((obj section)*standard-output*)
  (unless *print-escape*
    (dolist(string (section-body obj))
      (write-line string))))

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
			     :collect (make-common :body (cddr c)
						   :names (car c)
						   :alias (cadr c)
						   :path (Target-path(symbol-name(gensym "C_")))))
		       (loop :for s :in acc
			     :collect (make-single :body (cddr s)
						   :name (car s)
						   :path (Target-path(format nil "S_~A"(Replace-invalid-chars (car s))))))))
	      (BODY(sec rest acc)
		(let((pos (position-if (lambda(com)
					 (find (car sec)(car com)
					       :test #'string=))
				       commons)))
		  (if pos
		    (setf (nth pos commons)
			  (nconc (nth pos commons) sec))
		    (REC rest (push sec acc))))))
	(REC singles)))))
