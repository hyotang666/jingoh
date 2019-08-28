(in-package :jingoh.org)

(defvar *orgs* (make-hash-table :test #'eq))

(defparameter *org*(make-org))

(defmacro deforg(&whole whole name)
  (check-bnf:check-bnf(:whole whole)
    (name symbol))
  `(EVAL-WHEN(:LOAD-TOPLEVEL :COMPILE-TOPLEVEL :EXECUTE)
     (REGISTER-ORG ',name (MAKE-ORG :NAME ',name))))

(macrolet((!(n form)
	    `(RESIGNAL-BIND((TYPE-ERROR()'NOT-ORG
			      :API ',(nth n '(REGISTER-ORG DELETE-SUBJECT))))
	       ,form)))

  (defun register-org(name org)
    (check-type name symbol)
    (! 0(check-type org org))
    (setf(gethash name *orgs*)org))

  (defun delete-subject(subject-designator &optional(org *org*))
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
  (remhash (org-name(find-org org-designator))
	   *orgs*))

(defmacro in-org(&whole whole name)
  (check-bnf:check-bnf(:whole whole)
    (name symbol))
  `(EVAL-WHEN(:LOAD-TOPLEVEL :COMPILE-TOPLEVEL :EXECUTE)
     (RESIGNAL-BIND((MISSING-ORG()'MISSING-ORG :API 'IN-ORG))
       (SETF *ORG*(FIND-ORG ',name)))))

(defmacro requirements-about(&whole whole subject &rest option*)
  (check-bnf:check-bnf(:whole whole)
    (subject symbol)
    (option* keyword T))
  `(EVAL-WHEN(:LOAD-TOPLEVEL :COMPILE-TOPLEVEL :EXECUTE)
     (SETF (ORG-OPTIONS *ORG*) ',option*)
     (SETF (ORG-CURRENT-SUBJECTS *ORG*)(LIST ',subject))))

(add-new-option-key :as)
(add-new-option-key :doc-type)

(defmacro common-requirements-about(&whole whole
					   (&rest subject*)
					   &rest option*
					   &key(as (error "Keyword parameter :AS is required."))
					   &allow-other-keys)
  (check-bnf:check-bnf(:whole whole)
    (subject* symbol)
    (option* keyword T)
    (as symbol))
  `(EVAL-WHEN(:LOAD-TOPLEVEL :COMPILE-TOPLEVEL :EXECUTE)
     (SETF (ORG-OPTIONS *ORG*) ',option*)
     (SETF (ORG-CURRENT-SUBJECTS *ORG*) ',subject*)))

(macrolet((?!(form)
	    `(OR ,form
		 (WHEN ERRORP
		   (ERROR'MISSING-ORG :API 'FIND-ORG :DATUM ORG-DESIGNATOR)))))

  (defun find-org(org-designator &optional(errorp t))
    (typecase org-designator
      (org org-designator)
      (null (make-org))
      (t (?!(gethash org-designator *orgs*))))))

