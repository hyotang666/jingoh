(in-package :jingoh.org)

(defstruct (org (:copier nil))
  (name nil :type symbol :read-only t)
  (package *package* :type package :read-only t)
  (current-subjects `(nil) :type cons)
  (options nil :type list)
  (specifications
    (make-array 0 :fill-pointer 0 :adjustable t :element-type 'spec)
    :type vector))

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
  (requirements #() :type vector))

(defmethod print-object ((o org) *standard-output*)
  (if (null *print-escape*)
      (call-next-method)
      (print-unreadable-object (o *standard-output* :type t)
        (let ((count (org-requirements-count o)))
          (format t "~A~:[ ~D ~:*requirement~P~;~]" (org-name o) (zerop count)
                  count)))))

(defmethod print-object ((s spec) *standard-output*)
  (if (null *print-escape*)
      (call-next-method)
      (print-unreadable-object (s *standard-output* :type t)
        (let ((count (length (spec-requirements s))))
          (format t "~A~:[ ~D ~:*requirement~P~;~]" (spec-subject s)
                  (zerop count) count)))))

(deftype org-designator () '(or (and symbol (not boolean)) org))

(deftype subject-designator () 'symbol)

(defvar *option-keys* (make-hash-table :test #'eq))

(defun list-all-option-keys () (alexandria:hash-table-keys *option-keys*))

(defun add-new-option-key (key)
  (check-type key keyword)
  (setf (gethash key *option-keys*) key))

(defun find-option-key (key &optional (errorp t))
  (or (gethash key *option-keys*)
      (when errorp
        (error "Not found option key ~S." key))))

(defun delete-option-key (key) (remhash key *option-keys*))

(defun clear-option-keys () (clrhash *option-keys*))