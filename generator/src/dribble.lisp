(in-package #:jingoh.generator)

(defvar *spec-output* *standard-output*)

(defun dribble(system &optional package)
  (let((*default-pathname-defaults*
	 (Spec-directory system))
       (*package* (or package *package*)))
    (append-spec #'repl)))

(defun append-spec(appender)
  (with-open-file(*standard-output*
		   (make-pathname :name (string-downcase(package-name *package*))
				  :type "lisp")
		   :direction :output
		   :if-exists :append)
    (funcall appender)))

(defun dribble-read(&optional (*standard-input* *query-io*))
  (format t "~%DRIBBLE> ")
  (force-output)
  (read))

(defun dribble-eval(form)
  (let*((condition)
	(result)
	(output
	  (restart-bind((append-spec
			  (lambda()
			    (format *spec-output* "~%#?~S :signals ~S"
				    form
				    (type-of condition)))
			  :report-function
			  (lambda(s)
			    (format s "Append spec. This error is valid behavior."))))
	    (handler-bind((condition
			    (lambda(c)
			      (setq condition c))))
	      (with-output-to-string(*standard-output*)
		(setq result (multiple-value-list(eval form))))))))
    (when (typep condition 'warning)
      (format *spec-output* "~%#?~S :signals ~S"
	      form
	      (type-of condition)))
    (unless(equal "" output)
      (format *spec-output* "~%#?~S :outputs ~S"
	      form
	      output))
    (if(cdr result) ; multiple-value.
      (if(typep form '(cons (eql macroexpand-1)
			    *))
	(format *spec-output* "~%#?~S :expanded-to ~S"
		(cadr form)
		(car result))
	(if(some #'unreadable-objectp result)
	  (format *spec-output* "~%#?~S~%:multiple-value-satisfies~%~S"
		  form
		  `(lambda,(loop :for i :upfrom 1 :to (length result)
				 :collect (intern(format nil "RESULT~D" i)))
		     :TODO))
	  (format *spec-output* "~%#?~S~%:values ~S"
		  form
		  result)))
      (if(unreadable-objectp (car result))
	(format *spec-output* "~%#?~S :be-the ~S"
		form
		(type-of (car result)))
	(format *spec-output* "~%#?~S => ~S~@[~%~A~]~@[~%~A~]"
		form
		(car result)
		(when (typep condition 'warning)
		  ", :ignore-signals warning")
		(unless(equal "" output)
		  ", :stream nil"))))
	(force-output)
	(values-list result)))

(defun unreadable-objectp(object)
  (uiop:string-prefix-p "#<" (prin1-to-string object)))

(defun dribble-print(&rest values)
  (map nil #'print values)
  (values))
