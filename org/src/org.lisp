(in-package :jingoh.org)

(declaim (optimize speed))

(defstruct (org (:copier nil))
  (name nil :type symbol :read-only t)
  (package *package* :type package :read-only t)
  (current-subjects `(nil) :type cons)
  (options nil :type list)
  (specifications
    (make-array 0 :fill-pointer 0 :adjustable t :element-type 'spec)
    :type (vector spec *)))

(deftype requirement () 'cons)

(defstruct (spec (:copier nil)
                 (:predicate nil)
                 (:constructor spec
                  (subject req &aux
                   (requirements
                    (make-array 1
                                :fill-pointer 1
                                :adjustable t
                                :initial-contents (list req))))))
  (subject nil :type symbol :read-only t)
  (requirements #() :type (vector requirement *)))

(declaim
 (ftype (function (spec) (values (vector requirement *) &optional))
        spec-requirements))

(declaim
 (ftype (function (org) (values (mod #.most-positive-fixnum) &optional))
        org-requirements-count))

(defun org-requirements-count (org)
  #+clisp
  (assert (typep org 'org) ()
    'not-org :datum org
             :expected-type 'org
             :api 'org-requirements-count)
  (reduce #'+ (org-specifications org)
          :key (lambda (x) (length (spec-requirements x)))))

(defmethod print-object ((o org) *standard-output*)
  (if (null *print-escape*)
      (call-next-method)
      (print-unreadable-object (o *standard-output* :type t)
        (let ((count (org-requirements-count o)))
          (format t "~A~:[ ~D requirement~P~;~]" (org-name o) (zerop count)
                  count count)))))

(defmethod print-object ((s spec) *standard-output*)
  (if (null *print-escape*)
      (call-next-method)
      (print-unreadable-object (s *standard-output* :type t)
        (let ((count (length (spec-requirements s))))
          (format t "~A~:[ ~D requirement~P~;~]" (spec-subject s) (zerop count)
                  count count)))))

(deftype org-designator () '(or (and symbol (not boolean)) org))

(deftype subject-designator () 'symbol)

(defvar *option-keys* (make-hash-table :test #'eq))

(declaim (ftype (function nil (values list &optional)) list-all-option-keys))

(defun list-all-option-keys () (alexandria:hash-table-keys *option-keys*))

(declaim
 (ftype (function (keyword) (values keyword &optional)) add-new-option-key))

(defun add-new-option-key (key)
  #+clisp
  (check-type key keyword)
  (setf (gethash key *option-keys*) key))

(declaim
 (ftype (function (keyword &optional boolean)
         (values (or null keyword) &optional))
        find-option-key))

(defun find-option-key (key &optional (errorp t))
  (or (values (gethash key *option-keys*))
      (when errorp
        (error "Not found option key ~S." key))))

(declaim
 (ftype (function (keyword) (values boolean &optional)) delete-option-key))

(defun delete-option-key (key) (remhash key *option-keys*))

(declaim (ftype (function nil (values hash-table &optional)) clear-option-keys))

(defun clear-option-keys () (clrhash *option-keys*))