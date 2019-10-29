(in-package :jingoh.generator)

(defun spec-directory(system)
  (uiop:subpathname (asdf:system-source-directory (asdf:find-system system))
		    "spec/"))

(defun test-asd-path(system)
  (make-pathname :name (test-name system)
		 :type "asd"
		 :defaults *default-pathname-defaults*))

(defun test-name(system)
  (concatenate 'string (asdf:coerce-name system) ".test"))

(defun path-of(name type
		    &optional
		    (*default-pathname-defaults* *default-pathname-defaults*))
  (make-pathname :name name
		 :type type
		 :defaults *default-pathname-defaults*))
