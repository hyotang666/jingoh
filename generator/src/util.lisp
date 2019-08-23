(in-package :jingoh.generator)

(defun spec-directory(system)
  (uiop:subpathname (asdf:system-source-directory (asdf:find-system system))
		    "spec/"))

(defun test-asd-path(system)
  (make-pathname :name (test-name (asdf:coerce-name system))
		 :type "asd"
		 :defaults *default-pathname-defaults*))

(defun test-name(name)
  (concatenate 'string name ".test"))

