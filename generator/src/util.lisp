(in-package :jingoh.generator)

(declaim (optimize speed))

(declaim
 (ftype (function (system-designator) (values pathname &optional))
        spec-directory))

(defun spec-directory (system)
  (uiop:subpathname (asdf:system-source-directory (asdf:find-system system))
                    "spec/"))

(declaim
 (ftype (function (system-designator) (values string &optional)) test-name))

(defun test-name (system)
  (concatenate 'string (asdf:coerce-name system) ".test"))

(declaim
 (ftype (function (string string &optional pathname)
         (values pathname &optional))
        path-of))

(defun path-of
       (name type
        &optional (*default-pathname-defaults* *default-pathname-defaults*))
  (make-pathname :name name :type type :defaults *default-pathname-defaults*))

(declaim
 (ftype (function
         (pathname function &key
                   (:if-exists (member :supersede :append :error)))
         (values null &optional))
        output-to))

(defun output-to (path thunk &key (if-exists :supersede))
  (uiop:with-output-file (*standard-output* (ensure-directories-exist path)
                          :if-exists if-exists)
    (funcall thunk)))