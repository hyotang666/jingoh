(in-package :jingoh.generator)

(defun spec-directory(system)
  (uiop:subpathname (asdf:system-source-directory (asdf:find-system system))
		    "spec/"))

(defun test-name(system)
  (concatenate 'string (asdf:coerce-name system) ".test"))

(defun path-of(name type
		    &optional
		    (*default-pathname-defaults* *default-pathname-defaults*))
  (make-pathname :name name
		 :type type
		 :defaults *default-pathname-defaults*))

(defun output-to(path thunk)
  (uiop:with-output-file(*standard-output*(ensure-directories-exist path)
			  :if-exists :supersede)
    (funcall thunk)))
