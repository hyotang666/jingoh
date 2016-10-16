(in-package :jingoh.org)

(defmacro do-requirements((var &optional(subject-designator T)(org '*org*) return)&body body)
  #.(Doc :jingoh.org "doc/do-requirements.M.md")
  (let((gname(gensym "NAME"))
       (v(gensym "V")))
    `(macrolet((?!(form)
		`(OR ,form
		     (ERROR'MISSING-SUBJECT :API 'DO-REQUIREMENTS
					    :DATUM ,',subject-designator)))
	       (!(form)
		 `(RESIGNAL-BIND((TYPE-ERROR()'NOT-ORG :API 'DO-REQUIREMENTS))
		    ,FORM)))
      (LET((,gname ,subject-designator))
	 (CASE,gname
	   ((NIL)(LOOP :FOR (NIL . ,v) :ACROSS (!(ORG-SPECIFICATIONS ,org))
		       :DO (MAP NIL (LAMBDA(,var),@body),v)
		       :FINALLY(RETURN ,return)))
	   ((T)(BLOCK()
		 (MAP NIL (LAMBDA(,var),@body)
		      (CDR(?!(FIND *SUBJECT* (!(ORG-SPECIFICATIONS ,org))
				   :KEY #'CAR))))
		 ,return))
	   (OTHERWISE
	     (BLOCK()
	       (MAP NIL(LAMBDA(,var) ,@body)
		    (CDR(?!(FIND ,gname (!(ORG-SPECIFICATIONS ,org))
				 :KEY #'CAR))))
	       ,return)))))))

(macrolet((!(n form)
	    `(RESIGNAL-BIND((ERROR()'NOT-ORG
			      :DATUM ORG
			      :EXPECTED-TYPE 'ORG
			      :API ',(nth n '(MAP-REQUIREMENTS ADD-REQUIREMENT ORG-REQUIREMENTS-COUNT))))
	       ,form))
	  (?!(form)
	    `(OR ,form
		 (ERROR 'MISSING-SUBJECT :API 'map-topic :DATUM SUBJECT))))

  (defun map-requirements(function &optional(subject T)(org *org*))
    #++(Doc :jingoh.org "doc/map-topic.F.md")
    (case subject
      ((NIL) ; all subject.
       (loop :for (NIL . v) :across (! 0(org-specifications org))
	     :nconc(map 'list function v)))
      ((T) ; current subject
       (block() ; in order to keep same behavior.
	 (map 'list function(cdr(?!(find *subject*(! 0(org-specifications org))
					 :key #'car))))))
      (otherwise
	(block() ; in order to keep same behavior.
	  (map 'list function(cdr(?!(find subject (! 0(org-specifications org))
					  :key #'car))))))))

  (defun add-requirement(requirement &optional(org *org*))
    #.(Doc :jingoh.org "doc/add-requirement.F.md")
    (let((subject(find *subject* (! 1(org-specifications org))
		       :key #'car)))
      (if subject
	(vector-push-extend requirement(cdr subject))
	(vector-push-extend(cons *subject*
				 (make-array 1
					     :fill-pointer 1
					     :adjustable t
					     :initial-contents (list requirement)))
	  (org-specifications org))))
    requirement)

  (defun org-requirements-count(org)
    #.(Doc :jingoh.org "doc/org-requirements-count.F.md")
    (loop :for (NIL . v) :across (! 2(org-specifications org))
	  :sum (length v)))

  ) ; end of macrolet

