(in-package :jingoh.org)

(declaim (optimize speed))

(defvar *orgs* (make-hash-table :test #'eq))

(defparameter *org* (make-org))

(declaim (ftype (function (symbol org) (values org &optional)) register-org))

(defun register-org (name org)
  #+(or clisp allegro abcl)
  (progn (check-type name symbol) (check-type org org))
  (setf (gethash name *orgs*) org))

#+(or allegro)
(defun delete% (item vector &key (key #'identity))
  ;; DELETE for allegro.
  ;; Allegro wipe away fill-pointer after DELETE.
  (assert (array-has-fill-pointer-p vector))
  (loop :for index :upfrom 0 :below (fill-pointer vector)
        :with fill-pointer = 0
        :unless (eql item (funcall key (aref vector index)))
          :do (setf (aref vector fill-pointer) (aref vector index)) ; shift
              (incf fill-pointer)
        :finally (setf (fill-pointer vector) fill-pointer)
                 (return vector)))

(declaim
 (ftype (function (symbol &optional org) (values (eql t) &optional))
        delete-subject))

(defun delete-subject (subject-designator &optional (org *org*))
  (flet ((del-sub (sub)
           (setf (org-specifications org)
                   (#.(or #+(or allegro) 'delete% 'delete) sub
                    (org-specifications org) :key #'spec-subject))))
    (case subject-designator
      ((nil) ; delete all.
       (setf (fill-pointer (org-specifications org)) 0))
      ((t) ; delete current.
       (mapc #'del-sub (org-current-subjects org)))
      (otherwise ; delete specified one.
       (del-sub subject-designator))))
  t)

(defmacro deforg (&whole whole name)
  #.(if (alexandria:featurep :check-bnf)
        '(check-bnf:check-bnf (:whole whole) ((name symbol)))
        '(progn
          whole ; to muffle unused style warning.
          (check-type name symbol)))
  `(eval-when (:load-toplevel :compile-toplevel :execute)
     (register-org ',name (make-org :name ',name))))

(declaim
 (ftype (function (org-designator) (values boolean &optional)) delete-org))

(defun delete-org (org-designator)
  (remhash (org-name (find-org org-designator)) *orgs*))

(defmacro in-org (&whole whole name)
  #.(if (alexandria:featurep :check-bnf)
        '(check-bnf:check-bnf (:whole whole) ((name symbol)))
        '(progn
          whole ; to muffle unused style warning.
          (check-type name symbol)))
  `(eval-when (:load-toplevel :compile-toplevel :execute)
     (setf *org*
             (or (find-org ',name nil)
                 (error 'missing-org :api 'in-org :datum ',name)))))

(defmacro requirements-about (&whole whole subject &rest option*)
  #.(if (alexandria:featurep :check-bnf)
        '(check-bnf:check-bnf (:whole whole)
           ((subject symbol))
           ((option* keyword t)))
        '(progn
          whole ; to muffle unused style warning.
          (check-type subject symbol)
          (loop :for (k) :on option* :by #'cddr
                :do (check-type k keyword))))
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
  #.(if (alexandria:featurep :check-bnf)
        '(check-bnf:check-bnf (:whole whole)
           ((subject* symbol))
           ((option* keyword t))
           ((as symbol)))
        '(progn
          whole ; to muffle unused style warning.
          (assert (every #'symbolp subject*))
          (loop :for (k) :on option* :by #'cddr
                :do (check-type k keyword))
          (check-type as symbol)))
  `(eval-when (:load-toplevel :compile-toplevel :execute)
     (setf (org-options *org*) ',option*)
     (setf (org-current-subjects *org*) ',subject*)))

(declaim
 (ftype (function ((or null org-designator) &optional boolean)
         (values (or null org) &optional))
        find-org))

(defun find-org (org-designator &optional (errorp t))
  (typecase org-designator
    (org org-designator)
    (null (make-org))
    (t
     (or (values (gethash org-designator *orgs*))
         (when errorp
           (error 'missing-org :api 'find-org :datum org-designator))))))