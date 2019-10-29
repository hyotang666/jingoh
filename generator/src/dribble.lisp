(in-package #:jingoh.generator)

(defun dribble(system &optional package)
  (let((*default-pathname-defaults*
	 (Spec-directory system))
       (*package* (or package *package*)))
    (with-open-file(*standard-output*
		     (make-pathname :name (string-downcase(package-name *package*))
				    :type "lisp")
		     :direction :output
		     :if-exists :append)
      (repl))))

