(defpackage :jingoh.documentizer.parse-spec(:use :cl :read-as-string)
  (:import-from :jingoh.documentizer.utility
		#:Target-path
		#:Replace-invalid-chars)
  (:import-from :jingoh.documentizer.sections
		#:Make-common
		#:Make-single
		#:Single-p
		#:Section-names
		#:Section-body
		)
  (:export
    ; main api
    #:parse-spec
    ))
(in-package :jingoh.documentizer.parse-spec)

(defun parse-spec(pathname)
  (multiple-value-call #'engroup(sectionize(enlist pathname))))

(defun enlist(pathname)
  (with-open-file(s pathname)
    (loop :for exp = (Read-as-string s nil nil)
	  :while exp
	  :collect exp)))

(defun sectionize(list)
  (flet((section-p(elt)
	  (and (char= #\( (char elt 0))
	       (let*((*package*
		       (find-package :jingoh.documentizer.parse-spec))
		     (sexp
		       (read-from-string elt)))
		 (when(find (car sexp)
			    '(requirements-about common-requirements-about))
		   sexp)))))
    (loop :for list :on list
	  :with body
	  :for section = (section-p (car list))
	  :for alias = (getf section :as)
	  :if section
	  :do (setf body (subseq list 1
				 (position-if #'section-p list
					      :start 1)))
	  :and :if alias
	  :collect (Make-common :names (cadr section)
				:alias alias
				:doc-type (getf section :doc-type)
				:path (Target-path(symbol-name(gensym"C_")))
				:body body)
	  :into commons
	  :else :collect(Make-single
			  :name (cadr section)
			  :doc-type (getf section :doc-type)
			  :path (Target-path
				  (format nil "S_~A"
					  (Replace-invalid-chars (cadr section))))
			  :body body)
	  :into singles
	  :finally (return(values singles commons)))))

(defun engroup (singles commons)
  (nconc commons
	 (loop :for single :in singles
	       :for common = (find (car (Section-names single))
				   commons
				   :key #'Section-names
				   :test (lambda(name list)
					   (find name list :test #'string=)))
	       :if common
	       :do (setf (Section-body common)
			 (nconc (Section-body common)
				(cons (format nil "#| ~A |#"
					      (car(Section-names single)))
				      (Section-body single))))
	       :else collect single)))
