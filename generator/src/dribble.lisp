(in-package #:jingoh.generator)

(defvar *spec-output* *standard-output*)

(defvar *spec-append-hook* 'funcall)

(defun dribble(system &optional package)
  (let*((*package*
	  (or (when package
		(or (find-package package)
		    (error "Package missing: ~S" package)))
	      *package*))
	(*default-pathname-defaults*
	  (Path-of (string-downcase(package-name *package*))
		   "lisp"
		   (Spec-directory system)))
	(*spec-append-hook*
	  'spec-appender))
    (repl)))

(defun spec-appender(appender)
  (with-open-file(*spec-output* *default-pathname-defaults*
				:direction :output
				:if-exists :append)
    (funcall appender)))

(defun repl()
  (catch
    'quit
    (loop
      (restart-case(multiple-value-call #'dribble-print
		     (funcall *spec-append-hook*
			      (lambda()
				(handler-bind((append-spec #'append-spec))
				  (dribble-eval (dribble-read))))))
	(dribble()
	  :report "Return to dribble.")))))

(define-condition append-spec(simple-condition)
  ())

(defun append-spec(condition)
  (when(find-restart 'append-spec condition)
    (princ condition *spec-output*)
    (force-output *spec-output*)))

(defun dribble-read(&optional (*standard-input* *query-io*))
  (let((*standard-output*
	 *query-io*))
    (format t "~%DRIBBLE> ")
    (force-output)
    (read)))

(defgeneric spec-of(dispatcher form result))
(defmethod spec-of :around(a b c)
  (the (values null &optional)
       (call-next-method)))

(defun dribble-eval(form)
  (when(or (eq :q form)
	   (and (listp form)
		(string= 'dribble (car form))))
    (throw 'quit (values)))
  (let*((condition)
	(result)
	(output
	  (restart-case(handler-bind((condition
				       (lambda(c)
					 (setq condition c))))
			 (with-output-to-string(s)
			   (let((*standard-output*
				  (make-broadcast-stream *standard-output* s)))
			     (setq result (multiple-value-list(eval form))))))
	    (append-spec()
	      :report "Append spec, returning to dribble."
	      (signal 'append-spec
		      :format-control "~%#?~S :signals ~S"
		      :format-arguments (list form (type-of condition)))
	      (return-from dribble-eval (values))))))
    (shiftf +++ ++ + form)
    (shiftf *** ** * (car result))
    (shiftf /// // / result)
    (cond
      ((find form '(+++ ++ + *** ** * /// // /)
	     :test #'equal)) ; do nothing
      ((eq :G form)
       (let((symbol
	      (prompt-for:prompt-for 'symbol "~&>> ")))
	 (symbol-generate symbol (symbol-package symbol)))
       (setq result nil))
      (T
	(spec-of :condition form condition)
	(spec-of :output form output)
	(if(cdr result) ; multiple-value.
	  (if(typep form '(cons (member macroexpand-1 macroexpand)
				*))
	    (spec-of :expansion form (car result))
	    (spec-of :values form result))
	  (if(unreadable-objectp (car result))
	    (spec-of :unreadable form (car result))
	    (spec-of :default form (list* condition output result))))))
    (values-list result)))

(define-condition unexpected-behavior(error)
  ())

;;; SPEC-OF methods
(defmethod spec-of((d (eql :condition))form condition)
  (when (and (typep condition 'warning)
	     (y-or-n-p "Expected signals? ~S" condition))
    (format *spec-output* "~%#?~S :signals ~S"
	    form
	    (type-of condition))))

(defmethod spec-of((d (eql :output))form output)
  (unless(equal "" output)
    (format *spec-output* "~%#?~S :outputs ~S"
	    form
	    (if(y-or-n-p "Expected output?")
	      output
	      (restart-case(error 'unexpected-behavior)
		(use-value(expected)
		  :report "Specify expected output"
		  :interactive (lambda()
				 (list(read-expected)))
		  expected))))))

(defmethod spec-of((d (eql :expansion)) form result)
  (format *spec-output* "~%#?~S :expanded-to ~S"
	  (cadr form)
	  (if(y-or-n-p "~S~%Expected expansion?"result)
	    result
	    (restart-case(error 'unexpected-behavior)
	      (use-value(expected)
		:report "Specify expected expression."
		:interactive (lambda()
			       (list (prompt-for:prompt-for t "Input expected form. >> ")))
		expected)))))

(defmethod spec-of((d (eql :values)) form result)
  (if(some #'unreadable-objectp result)
    (format *spec-output* "~%#?~S~%:multiple-value-satisfies~%~S"
	    form
	    `(lambda,(loop :for i :upfrom 1 :to (length result)
			   :collect (intern(format nil "RESULT~D" i)))
	       :TODO))
    (format *spec-output* "~%#?~S~%:values ~S"
	    form
	    (if(y-or-n-p "~{~S~%~}Expected values?" result)
	      result
	      (restart-case(error 'unexpected-behavior)
		(use-value(expected)
		  :report "Specify expected values as list."
		  :interactive (lambda()
				 (list(prompt-for:prompt-for
					'list
					"Input expected values. >> ")))
		  expected))))))

(defmethod spec-of((d (eql :unreadable))form result)
  (format *spec-output* "~%#?~S :be-the ~S"
	  form
	  (if(y-or-n-p "Expected type? ~S" result)
	    (type-of result)
	    (restart-case(error 'unexpected-behavior)
	      (use-value(expected)
		:report "Specify expected type."
		:interactive (lambda()
			       (list (prompt-for:prompt-for t "Input expected type. >> ")))
		expected)))))

(defmethod spec-of((d (eql :default))form args)
  (destructuring-bind(condition output result)args
    (format *spec-output* "~%#?~S => ~S~@[~%~A~]~@[~%~A~]"
	    form
	    (if(y-or-n-p "~S~%Expected result?"result)
	      result
	      (restart-case(error 'unexpected-behavior)
		(use-value(expected)
		  :report "Specify expected value."
		  :interactive (lambda()
				 (list (prompt-for:prompt-for
					 t
					 "Input expected result. >> ")))
		  expected)))
	    (when (typep condition 'warning)
	      ", :ignore-signals warning")
	    (unless(equal "" output)
	      ", :stream nil"))))

(defun unreadable-objectp(object)
  (uiop:string-prefix-p "#<" (prin1-to-string object)))

(defun read-expected()
  (format *query-io* "Type expected output till Ctl-D.~%")
  (force-output *query-io*)
  (loop :for line := (read-line *query-io* nil nil)
	:while line
	:collect line :into lines
	:finally (return (format nil "~{~A~^~%~}" lines))))

(defun dribble-print(&rest values)
  (map nil #'print values)
  (force-output)
  (values))
