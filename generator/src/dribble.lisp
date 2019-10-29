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

(defun dribble-read(&optional (*standard-input* *query-io*))
  (write-string "DRIBBLE> ")
  (force-output)
  (read))

(defun unreadable-objectp(object)
  (uiop:string-prefix-p "#<" (prin1-to-string object)))
