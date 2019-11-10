(in-package :jingoh.documentizer)

(defun parse-spec(pathname)
  (multiple-value-call #'engroup(sectionize(enlist pathname))))

(defun enlist(pathname)
  (with-open-file(s pathname)
    (loop :for exp = (let((read-as-string:*muffle-reader-error*
			    T))
		       (read-as-string:read-as-string s nil nil))
	  :while exp
	  :collect exp)))

(defun sectionize(list)
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
					(replace-invalid-chars (cadr section))))
			:body body)
	:into singles
	:finally (return(values singles commons))))

(defun section-p(elt)
  (and (char= #\( (char elt 0))
       (let*((*package*
	       (find-package :jingoh.documentizer))
	     (null-package:*only-junk-p*
	       T)
	     (sexp
	       (with-input-from-string(s elt)
		 (null-package:read-with-null-package s))))
	 (when(find (car sexp)
		    '(requirements-about common-requirements-about))
	   sexp))))

(defun replace-invalid-chars(arg)
  (loop :for c :across (string-downcase(string arg))
	:for n :upfrom 0
	:when (and (not(alphanumericp c))
		   (not(char= #\. c)))
	:collect (princ-to-string(char-code c)) :into result
	:else :collect c :into result
	:finally(return (uiop:reduce/strcat result))))

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
