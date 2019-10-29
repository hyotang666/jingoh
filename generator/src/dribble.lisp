(in-package #:jingoh.generator)

(defvar *spec-output* *standard-output*)

(defvar *spec-append-hook* 'funcall)

(defun dribble(system &optional package)
  (let((*default-pathname-defaults*
	 (Spec-directory system))
       (*package* (or package *package*)))
    (with-open-file(*spec-output*
		     (make-pathname :name (string-downcase(package-name *package*))
				    :type "lisp")
		     :direction :output
		     :if-exists :append)
      (repl))))

(defun spec-appender(appender)
  (with-open-file(*spec-output* *default-pathname-defaults*
				:direction :output
				:if-exists :append)
    (funcall appender)))

(defun repl()
  (catch
    'quit
    (loop
      (restart-case(map nil
			(lambda(x)
			  (print x)
			  (force-output))
			(multiple-value-list(funcall *spec-append-hook*
						     (lambda()
						       (dribble-eval (dribble-read))))))
	(dribble()
	  :report "Return to dribble.")))))

(defun dribble-read(&optional (*standard-input* *query-io*))
  (let((*standard-output*
	 *query-io*))
    (format t "~%DRIBBLE> ")
    (force-output)
    (read)))

(defun dribble-eval(form)
  (when(string= 'dribble (car(uiop:ensure-list form)))
    (throw 'quit (values)))
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
	      (with-output-to-string(s)
		(let((*standard-output*
		       (make-broadcast-stream *standard-output* s)))
		  (setq result (multiple-value-list(eval form)))))))))
    (shiftf +++ ++ + form)
    (shiftf *** ** * (car result))
    (shiftf /// // / result)
    (unless(find form '(+++ ++ + *** ** * /// // /)
		 :test #'equal)
      (spec-of-warns form condition)
      (spec-of-output form output)
      (if(cdr result) ; multiple-value.
	(if(typep form '(cons (eql macroexpand-1)
			      *))
	  (format *spec-output* "~%#?~S :expanded-to ~S"
		  (cadr form)
		  (if(y-or-n-p "Expected expansion? ~S"(car result))
		    (car result)
		    (prompt-for:prompt-for t "Input expected form. >> ")))
	  (if(some #'unreadable-objectp result)
	    (format *spec-output* "~%#?~S~%:multiple-value-satisfies~%~S"
		    form
		    `(lambda,(loop :for i :upfrom 1 :to (length result)
				   :collect (intern(format nil "RESULT~D" i)))
		       :TODO))
	    (format *spec-output* "~%#?~S~%:values ~S"
		    form
		    (if(y-or-n-p "Expected values? ~S" result)
		      result
		      (prompt-for:prompt-for 'list "Input expected values. >> ")))))
	(if(unreadable-objectp (car result))
	  (format *spec-output* "~%#?~S :be-the ~S"
		  form
		  (if(y-or-n-p "Expected type? ~S" (car result))
		    (type-of (car result))
		    (prompt-for:prompt-for t "Input expected type. >> ")))
	  (format *spec-output* "~%#?~S => ~S~@[~%~A~]~@[~%~A~]"
		  form
		  (if(y-or-n-p "Expected result? ~S"(car result))
		    (car result)
		    (prompt-for:prompt-for t "Input expected result. >> "))
		  (when (typep condition 'warning)
		    ", :ignore-signals warning")
		  (unless(equal "" output)
		    ", :stream nil")))))
    (force-output)
    (values-list result)))

(defun spec-of-warns(form condition)
  (when (and (typep condition 'warning)
	     (y-or-n-p "Expected signals? ~S" condition))
    (format *spec-output* "~%#?~S :signals ~S"
	    form
	    (type-of condition))))

(defun spec-of-output(form output)
  (unless(equal "" output)
    (format *spec-output* "~%#?~S :outputs ~S"
	    form
	    (if(y-or-n-p "Expected output? ~S" output)
	      output
	      (read-expected)))))

(defun unreadable-objectp(object)
  (uiop:string-prefix-p "#<" (prin1-to-string object)))

(defun read-expected()
  (format *query-io* "Type expected output till Ctl-D.~%")
  (force-output *query-io*)
  (loop :for line := (read-line *query-io* nil nil)
	:while line
	:collect line :into lines
	:finally (return (format nil "~{~A~^~%~}" lines))))
