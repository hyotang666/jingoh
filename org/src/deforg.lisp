(in-package :jingoh.org)

(defvar *orgs* (make-hash-table :test #'eq))

(defparameter *org*(make-org)
  #.(Doc :jingoh.org "doc/AorgA.V.md"))

(define-symbol-macro *subject* (ORG-CURRENT-SUBJECT *ORG*))
(define-symbol-macro *options* (ORG-OPTIONS *ORG*))

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
  #.(Doc :jingoh.org "doc/delete-org.F.md")
  (remhash (org-name(find-org org-designator))
	   *orgs*))

(defmacro in-org(name)
  #.(Doc :jingoh.org "doc/in-org.M.md")
  (check-type name symbol)
  `(EVAL-WHEN(:LOAD-TOPLEVEL :COMPILE-TOPLEVEL :EXECUTE)
     (RESIGNAL-BIND((MISSING-ORG()'MISSING-ORG :API 'IN-ORG))
       (SETF *ORG*(FIND-ORG ',name)))))

(defmacro requirements-about(subject &rest options)
  #.(Doc :jingoh.org "doc/requirements-about.M.md")
  (check-type subject symbol)
  `(EVAL-WHEN(:LOAD-TOPLEVEL :COMPILE-TOPLEVEL :EXECUTE)
     (SETF *OPTIONS* ',options)
     (SETF *SUBJECT* ',subject)))

(macrolet((?!(form)
	    `(OR ,form
		 (WHEN ERRORP
		   (ERROR'MISSING-ORG :API 'FIND-ORG :DATUM ORG-DESIGNATOR)))))

  (defun find-org(org-designator &optional(errorp t))
    #.(Doc :jingoh.org "doc/find-org.F.md")
    (if(org-p org-designator)
      org-designator
      (?!(gethash org-designator *orgs*)))))

