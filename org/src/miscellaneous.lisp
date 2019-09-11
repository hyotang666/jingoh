(in-package :jingoh.org)

(defun the-nil-subject-procedure(org var body return)
  ; iterate all requirements
  (let((s(gensym "S"))
       (sub?(cadr var)))
    `(LOOP :FOR ,s :ACROSS (!(ORG-SPECIFICATIONS ,org))
	   ,@(when sub?
	       `(:for ,sub? = (SPEC-SUBJECT ,s)))
	   :DO (MAP NIL (LAMBDA(,(car var)),@body)(SPEC-REQUIREMENTS ,s))
	   :FINALLY(RETURN (LET(,@var)
			     (DECLARE(IGNORABLE ,@var))
			     ,return)))))

(defun the-subject-procedure(var body gname org return)
  (alexandria:with-unique-names(specifications subject)
    `(LET((,specifications(!(ORG-SPECIFICATIONS ,org))))
       (DOLIST(,(or (cadr var)subject)(UIOP:ENSURE-LIST ,gname)
		(LET(,(car var))
		  (DECLARE(IGNORABLE ,(car var)))
		  ,return))
	 (MAP NIL (LAMBDA(,(car var)),@body)
	      (SPEC-REQUIREMENTS(?!(FIND ,(or (cadr var)subject)
					 ,specifications
					 :KEY #'SPEC-SUBJECT))))))))

(defmacro do-requirements(&whole whole
				 (var &optional(subject-designator T)
				      (org '*org*)
				      return-form)
				 &body body)
  (check-bnf:check-bnf(:whole whole)
    ((var (or symbol subject-spec))
     (subject-spec (symbol symbol)))
    ((subject-designator symbol))
    ((org check-bnf:expression))
    ((return-form check-bnf:expression)))
  (setf var (uiop:ensure-list var))
  (let((gname(gensym "NAME")))
    `(MACROLET((?!(form)
		 `(OR ,form
		      (ERROR'MISSING-SUBJECT :API 'DO-REQUIREMENTS
					     :DATUM ,',gname)))
	       (!(form)
		 `(RESIGNAL-BIND((TYPE-ERROR()'NOT-ORG :API 'DO-REQUIREMENTS))
		    ,FORM)))
       (LET((,gname ,subject-designator))
	 (CASE,gname
	   ((NIL),(the-nil-subject-procedure org var body return-form))
	   ((T),(the-subject-procedure var body `(SETF ,gname (ORG-CURRENT-SUBJECTS *ORG*)) org return-form))
	   (OTHERWISE,(the-subject-procedure var body gname org return-form)))))))

(define-compiler-macro do-requirements(&whole whole (var &optional(subject-designator T)(org '*org*) return)&body body)
  (setf var (uiop:ensure-list var))
  (if(not(constantp subject-designator))
    whole
    `(MACROLET((?!(form)
		 `(OR ,form
		      (ERROR'MISSING-SUBJECT :API 'DO-REQUIREMENTS
					     :DATUM ,',subject-designator)))
	       (!(form)
		 `(RESIGNAL-BIND((TYPE-ERROR()'NOT-ORG :API 'DO-REQUIREMENTS))
		    ,FORM)))
       ,(case subject-designator
	  ((NIL)(the-nil-subject-procedure org var body return))
	  ((T)(the-subject-procedure var body '(ORG-CURRENT-SUBJECTS *ORG*) org return))
	  (otherwise
	    (the-subject-procedure var body subject-designator org return))))))

(macrolet((!(n form)
	    `(RESIGNAL-BIND((ERROR()'NOT-ORG
			      :DATUM ORG
			      :EXPECTED-TYPE 'ORG
			      :API ',(nth n '(MAP-REQUIREMENTS ADD-REQUIREMENT ORG-REQUIREMENTS-COUNT))))
	       ,form))
	  (?!(form)
	    `(OR ,form
		 (ERROR 'MISSING-SUBJECT :API 'map-requirements :DATUM SUB))))

  (defun map-requirements(function &optional(subject T)(org *org*))
    (flet((s-reqs(sub)
	    (spec-requirements(?!(find sub(! 0 (org-specifications org))
				       :key #'spec-subject)))))
      (case subject
	((NIL) ; all subject.
	 (loop :for spec :across (! 0(org-specifications org))
	       :nconc(map 'list function (spec-requirements spec))))
	((T) ; current subject
	 (loop :for sub :in (org-current-subjects *org*)
	       :nconc (map 'list function(s-reqs sub))))
	(otherwise
	  (map 'list function(s-reqs subject))))))

  (defun add-requirement(subject requirement &optional(org *org*))
    (check-type subject symbol)
    (let((spec (find subject (! 1 (org-specifications org))
				    :key #'spec-subject)))
      (if spec
	(vector-push-extend requirement (spec-requirements spec))
	(vector-push-extend (spec subject requirement)
			    (org-specifications org))))
    requirement)

  (defun org-requirements-count(org)
    (loop :for s :across (! 2(org-specifications org))
	  :sum (length (spec-requirements s))))

  ) ; end of macrolet

(defun find-subject(subject &optional(org *org*))
  (loop :for s :across (org-specifications org)
	:when (eq subject (spec-subject s))
	:return s))
