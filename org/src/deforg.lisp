(in-package :jingoh.org)

(defvar *orgs* (make-hash-table :test #'eq))

(defparameter *org*(make-org)
  #.(Doc :jingoh.org "doc/AorgA.V.md"))

(defmacro deforg(name)
  #.(Doc :jingoh.org "doc/deforg.M.md")
  (check-type name symbol)
  `(EVAL-WHEN(:LOAD-TOPLEVEL :COMPILE-TOPLEVEL :EXECUTE)
     (REGISTER-ORG ',name (MAKE-ORG :NAME ',name))))

(macrolet((!(n form)
	    `(RESIGNAL-BIND((TYPE-ERROR()'NOT-ORG
			      :API ',(nth n '(REGISTER-ORG DELETE-SUBJECT))))
	       ,form)))

  (defun register-org(name org)
    #.(Doc :jingoh.org "doc/register-org.F.md")
    (check-type name symbol)
    (! 0(check-type org org))
    (setf(gethash name *orgs*)org))

  (defun delete-subject(subject-designator &optional(org *org*))
    #.(Doc :jingoh.org "doc/delete-subject.F.md")
    (flet((del-sub(sub)
	    (setf (org-specifications org)
		  (delete sub (! 1 (org-specifications org))
			  :key #'spec-subject))))
      (case subject-designator
	((nil) ; delete all.
	 (loop :with spec = (! 1(org-specifications org))
	       :repeat (fill-pointer spec)
	       :do (vector-pop spec)))
	((T) ; delete current.
	 (mapc #'del-sub (org-current-subjects org)))
	(otherwise ; delete specified one.
	  (del-sub subject-designator))))
    T)

  ) ; end of macrolet

(defun delete-org(org-designator)
  #.(Doc :jingoh.org "doc/delete-org.F.md")
  (remhash (org-name(find-org org-designator))
	   *orgs*))

(defmacro in-org(name)
  #.(Doc :jingoh.org "doc/in-org.M.md")
  (check-type name symbol)
  `(EVAL-WHEN(:LOAD-TOPLEVEL :COMPILE-TOPLEVEL :EXECUTE)
     (RESIGNAL-BIND((MISSING-ORG()'MISSING-ORG :API 'IN-ORG))
       (SETF *ORG*(FIND-ORG ',name)))))

(defmacro requirements-about(subject &rest option*)
  #.(Doc :jingoh.org "doc/requirements-about.M.md")
  (check-type subject symbol)
  `(EVAL-WHEN(:LOAD-TOPLEVEL :COMPILE-TOPLEVEL :EXECUTE)
     (SETF (ORG-OPTIONS *ORG*) ',option*)
     (SETF (ORG-CURRENT-SUBJECTS *ORG*)(LIST ',subject))))

(defmacro common-requirements-about((&rest subject*) &rest option* &key(as (error "Keyword parameter :AS is required.")))
  (check-type as symbol)
  (assert (every #'symbolp subject*))
  `(EVAL-WHEN(:LOAD-TOPLEVEL :COMPILE-TOPLEVEL :EXECUTE)
     (SETF (ORG-OPTIONS *ORG*) ',option*)
     (SETF (ORG-CURRENT-SUBJECTS *ORG*) ',subject*)))

(macrolet((?!(form)
	    `(OR ,form
		 (WHEN ERRORP
		   (ERROR'MISSING-ORG :API 'FIND-ORG :DATUM ORG-DESIGNATOR)))))

  (defun find-org(org-designator &optional(errorp t))
    #.(Doc :jingoh.org "doc/find-org.F.md")
    (typecase org-designator
      (org org-designator)
      (null (make-org))
      (t (?!(gethash org-designator *orgs*))))))

