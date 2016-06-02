(in-package :jingoh.org)

(defvar *orgs* (make-hash-table :test #'eq))

(defparameter *org*(make-org)
  #.(doc :jingoh.org "doc/org/AorgA.V.md"))

(define-symbol-macro *subject* (ORG-CURRENT-SUBJECT *ORG*))

(defmacro deforg(name)
  #.(doc :jingoh.org "doc/org/deforg.M.md")
  (check-type name symbol)
  `(EVAL-WHEN(:LOAD-TOPLEVEL :COMPILE-TOPLEVEL :EXECUTE)
     (REGISTER-ORG ',name (MAKE-ORG :NAME ',name))))

(macrolet((!(n form)
	    `(WITH-RESIGNAL((TYPE-ERROR()'NOT-ORG
			      :API ',(nth n '(REGISTER-ORG DELETE-SUBJECT))))
	       ,form)))

  (defun register-org(name org)
    #.(doc :jingoh.org "doc/org/register-org.F.md")
    (check-type name symbol)
    (! 0(check-type org org))
    (setf(gethash name *orgs*)org))

  (defun delete-subject(subject-designator &optional(org *org*))
    #.(doc :jingoh.org "doc/org/delete-subject.F.md")
    (case subject-designator
      ((nil) ; delete all.
       (loop :with spec = (! 1(org-specifications org))
	     :repeat (fill-pointer spec)
	     :do (vector-pop spec)))
      ((T) ; delete current.
       (setf (org-specifications org)
	     (delete *subject* (! 1 (org-specifications org))
		     :key #'car)))
      (otherwise ; delete specified.
	(setf (org-specifications org)
	      (delete subject-designator (! 1(org-specifications org))
		      :key #'car))))
    T)

  ) ; end of macrolet

(defun delete-org(org-designator)
  #.(doc :jingoh.org "doc/org/delete-org.F.md")
  (remhash (org-name(find-org org-designator))
	   *orgs*))

(defmacro in-org(name)
  #.(doc :jingoh.org "doc/org/in-org.M.md")
  (check-type name symbol)
  `(EVAL-WHEN(:LOAD-TOPLEVEL :COMPILE-TOPLEVEL :EXECUTE)
     (WITH-RESIGNAL((MISSING-ORG()'MISSING-ORG :API 'IN-ORG))
       (SETF *ORG*(FIND-ORG ',name)))))

(defmacro requirements-about(subject)
  #.(doc :jingoh.org "doc/org/requirements-about.M.md")
  (check-type subject symbol)
  `(EVAL-WHEN(:LOAD-TOPLEVEL :COMPILE-TOPLEVEL :EXECUTE)
     (SETF *SUBJECT* ',subject)))

(macrolet((?!(form)
	    `(OR ,form
		 (WHEN ERRORP
		   (ERROR'MISSING-ORG :API 'FIND-ORG :DATUM ORG-DESIGNATOR)))))

  (defun find-org(org-designator &optional(errorp t))
    #.(doc :jingoh.org "doc/org/find-org.F.md")
    (if(org-p org-designator)
      org-designator
      (?!(gethash org-designator *orgs*)))))

