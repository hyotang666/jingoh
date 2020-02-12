(in-package :jingoh.org)

(defvar *orgs* (make-hash-table :test #'eq))

(defparameter *org* (make-org))

(defmacro deforg (&whole whole name)
  (check-bnf:check-bnf (:whole whole) ((name symbol)))
  `(eval-when (:load-toplevel :compile-toplevel :execute)
     (register-org ',name (make-org :name ',name))))

(macrolet ((! (n form)
             `(resignal-bind ((type-error () 'not-org
                                :api ',(nth n '(register-org delete-subject))))
                ,form)))
  (defun register-org (name org)
    (check-type name symbol)
    (! 0 (check-type org org))
    (setf (gethash name *orgs*) org))
  (defun delete-subject (subject-designator &optional (org *org*))
    (flet ((del-sub (sub)
             (setf (org-specifications org)
                     (delete sub (! 1 (org-specifications org))
                             :key #'spec-subject))))
      (case subject-designator
        ((nil) ; delete all.
         (loop :with spec = (! 1 (org-specifications org))
               :repeat (fill-pointer spec)
               :do (vector-pop spec)))
        ((t) ; delete current.
         (mapc #'del-sub (org-current-subjects org)))
        (otherwise ; delete specified one.
         (del-sub subject-designator))))
    t)) ; end of macrolet

(defun delete-org (org-designator)
  (remhash (org-name (find-org org-designator)) *orgs*))

(defmacro in-org (&whole whole name)
  (check-bnf:check-bnf (:whole whole) ((name symbol)))
  `(eval-when (:load-toplevel :compile-toplevel :execute)
     (resignal-bind ((missing-org () 'missing-org :api 'in-org))
       (setf *org* (find-org ',name)))))

(defmacro requirements-about (&whole whole subject &rest option*)
  (check-bnf:check-bnf (:whole whole)
    ((subject symbol))
    ((option* keyword t)))
  `(eval-when (:load-toplevel :compile-toplevel :execute)
     (setf (org-options *org*) ',option*)
     (setf (org-current-subjects *org*) (list ',subject))))

(add-new-option-key :as)

(add-new-option-key :doc-type)

(defmacro common-requirements-about
          (&whole whole (&rest subject*)
           &rest option*
           &key (as (error "Keyword parameter :AS is required."))
           &allow-other-keys)
  (check-bnf:check-bnf (:whole whole)
    ((subject* symbol))
    ((option* keyword t))
    ((as symbol)))
  `(eval-when (:load-toplevel :compile-toplevel :execute)
     (setf (org-options *org*) ',option*)
     (setf (org-current-subjects *org*) ',subject*)))

(macrolet ((?! (form)
             `(or ,form
                  (when errorp
                    (error 'missing-org
                           :api 'find-org
                           :datum org-designator)))))
  (defun find-org (org-designator &optional (errorp t))
    (typecase org-designator
      (org org-designator)
      (null (make-org))
      (t (?! (gethash org-designator *orgs*))))))