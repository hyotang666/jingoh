(in-package #:jingoh.documentizer)

(declaim (optimize speed))

;;;; COMPILE

(defun compile (system &optional (*print-example* *print-example*))
  "Compile spec documentation to lisp file."
  (let* ((system (ensure-system system))
         (sys-dir (asdf:system-source-directory system))
         (meta-datas (meta-datas<=system system sys-dir))
         (*default-pathname-defaults* sys-dir)
         (*package* #.(find-package :jingoh.documentizer)))
    (with-output-to ((merge-pathnames "doc.lisp"))
      (dolist (meta meta-datas)
        (print `(in-package ,(meta-data-name meta)))
        (dosections (s meta)
          (map nil #'print (<documentations> s (meta-data-name meta))))))))

(defun ensure-system (system)
  (restart-case (asdf:find-system system)
    (use-value (correct)
        :report "Specify correct system name."
        :interactive (lambda ()
                       (format *query-io* "~&>> ")
                       (force-output *query-io*)
                       (list (read *query-io*)))
      (ensure-system correct))))

(define-condition no-doc-type (style-warning)
  ((name :initarg :name :reader name))
  (:report
   (lambda (condition stream)
     (format stream "No doc type for ~A." (name condition)))))

(defun no-doc-type (name) (warn 'no-doc-type :name name))

(defun <documentations> (section package)
  (let ((doc-type (section-doc-type section)))
    (cond
      ((eq :unbound doc-type)
       (donames (name section)
         (no-doc-type name)))
      ((null doc-type))
      (t
       (uiop:while-collecting (collect)
         (donames (name section)
           (collect
            `(defmethod documentation
                        ((s
                          (eql
                            (uiop:find-symbol* ,(symbol-name name)
                                               ,(symbol-name package))))
                         (type (eql ',doc-type)))
               (declare (ignore s type))
               ,(princ-to-string section)))))))))

;;;; IMPORT

(defvar *import-hook* 'importer)

(defun import (system &optional (*print-example* *print-example*))
  "Import spec documentation to lisp image."
  (dolist (m (meta-datas<=system (ensure-system system)))
    (dosections (s m)
      (let ((doc-type (section-doc-type s)))
        (case doc-type
          ((nil))
          (:unbound
           (donames (name s)
             (no-doc-type name)))
          (otherwise
           (donames (name s)
             (funcall (coerce *import-hook* 'function)
                      (uiop:find-symbol* (symbol-name name) (meta-data-name m))
                      doc-type s))))))))

(defun importer (symbol doc-type section)
  (setf (documentation symbol doc-type) (princ-to-string section)))

(pushnew 'no-doc-type uiop:*uninteresting-conditions*)