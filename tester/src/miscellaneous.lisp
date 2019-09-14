(in-package :jingoh.tester)

(defun ignore-signals(type params)
  (let((condition
	 (getf params :ignore-signals)))
    (or (eq type condition)
	(eq T condition))))

(defun function-designator-p(symbol)
  (and (symbolp symbol)
       (fboundp symbol)
       (not (macro-function symbol))
       (not (special-operator-p symbol))))

(defun encallable(form &optional not-first-p)
  (typecase form
    (SYMBOL
      (if not-first-p
	`(FUNCTION ,form)
	form))
    (FUNCTION
      (if not-first-p
	form
	(or (millet:function-name form)
	    (function-lambda-expression form)
	    `(LAMBDA(&REST ARGS)
	       (APPLY ,form ARGS)))))
    ((CONS (EQL LAMBDA) T)
     form)
    ((OR (CONS (EQL FUNCTION)(CONS SYMBOL NULL))
	 (CONS (EQL QUOTE)(CONS SYMBOL NULL)))
     (if not-first-p
       `(function ,(cadr form))
       (second form)))
    (T (error 'syntax-error
	      :format-control "?: ~S is not function name"
	      :format-arguments (list form)))))

(define-condition syntax-error(simple-error program-error)())

(defun reserved-keywords(gf)
  (loop :for method :in (c2mop:generic-function-methods gf)
	:collect (c2mop:eql-specializer-object
		   (second(c2mop:method-specializers method)))
	:into result
	:finally (return (delete-duplicates result))))

(macrolet((defs(&body keys)
	    `(progn ,@(mapcar (lambda(key)
				`(jingoh.org:add-new-option-key ,key))
			      keys))))
  (defs :test
	:lazy
	:ignore-signals
	:with-restarts
	:stream
	:before
	:after
	:around
	:position
	:timeout))

(defun canonicalize(test-form parameters)
  (setf test-form (copy-tree test-form))
  (labels((CHECK()
	    (loop :for key :in parameters :by #'cddr
		  :do (resignal-bind:resignal-bind
			((error(c)
			   'simple-error
			   :format-control
			   (concatenate 'string
					(simple-condition-format-control c)
					"in ~S~&Allowed are ~S.")
			   :format-arguments
			   (append (simple-condition-format-arguments c)
				   (list parameters
					 (jingoh.org:list-all-option-keys)))))
			(jingoh.org:find-option-key key))))
	  (MAKE-BODY(test-form)
	    (SET-AROUND (let((after
			       (getf parameters :after)))
			  (if after
			    `(UNWIND-PROTECT ,(MAKE-PRIMARY test-form)
					     ,after)
			    (MAKE-PRIMARY test-form)))))
	  (MAKE-PRIMARY(test-form)
	    (let((before
		   (getf parameters :before))
		 (ignore-output-p
		   (null(getf parameters :stream '#:not-specified)))
		 (time(getf parameters :timeout 1)))
	      (if before
		(if ignore-output-p
		  `(LET((*STANDARD-OUTPUT*
			  (MAKE-BROADCAST-STREAM)))
		     (WITH-INTEGRATED-OUTPUT-STREAM(*STANDARD-OUTPUT*)
		       ,before
		       ,(MAY-MAKE-TIMEOUT-FORM time test-form)))
		  `(PROGN ,before
			  ,(MAY-MAKE-TIMEOUT-FORM time test-form)))
		(if ignore-output-p
		  `(LET((*STANDARD-OUTPUT*
			  (MAKE-BROADCAST-STREAM)))
		     (WITH-INTEGRATED-OUTPUT-STREAM(*STANDARD-OUTPUT*)
		       ,(MAY-MAKE-TIMEOUT-FORM time test-form)))
		  (MAY-MAKE-TIMEOUT-FORM time test-form)))))
	  (MAY-MAKE-TIMEOUT-FORM(time test-form)
	    (if bt:*supports-threads-p*
	      `(BT:WITH-TIMEOUT(,time)
		 ,test-form)
	      test-form))
	  (SET-AROUND(body)
	    (let((around
		   (getf parameters :around)))
	      (if around
		(subst body '(CALL-BODY) around :test #'equal)
		body)))
	  )
    (CHECK)
    (case(getf parameters :lazy :does-not-exist)
      (:does-not-exist
	(MAKE-BODY test-form))
      ((NIL)
       `(UIOP:CALL-WITH-MUFFLED-CONDITIONS
	  (LAMBDA (),(make-body test-form))
	  UIOP:*USUAL-UNINTERESTING-CONDITIONS*))
      (otherwise
	(make-body `(EVAL(MACROEXPAND ',test-form)))))))

(defun sexp=(sexp1 sexp2)
  (let(env)
    (labels((rec(sexp1 sexp2)
	      (typecase sexp1
		(cons (and (consp sexp2)
			   (rec (car sexp1)(car sexp2))
			   (rec (cdr sexp1)(cdr sexp2))))
		((or boolean keyword)
		 (eq sexp1 sexp2))
		(symbol (and (symbolp sexp2)
			     (if(symbol-package sexp1)
			       (eq sexp1 sexp2)
			       (let((pair
				      (assoc sexp1 env :test #'eq)))
				 (if pair
				   (eq sexp2 (cdr pair))
				   (let((pair
					  (rassoc sexp2 env :test #'eq)))
				     (unless pair
				       (push (cons sexp1 sexp2) env)
				       T)))))))
		((or string number character bit-vector pathname)
		 (equal sexp1 sexp2))
		(vector (and (vectorp sexp2)
			     (loop :for elt1 :across sexp1
				   :for elt2 :across sexp2
				   :always (rec elt1 elt2))))
		(array (and (arrayp sexp2)
			    (equal (array-dimensions sexp1)
				   (array-dimensions sexp2))
			    (dotimes(i (array-total-size sexp1)T)
			      (unless (rec (row-major-aref sexp1 i)
					   (row-major-aref sexp2 i))
				(return NIL)))))
		(t (if (typep(class-of sexp1)'structure-class)
		     (and (eq (type-of sexp1)(type-of sexp2))
			  (loop :for slot1 :in (slots<=obj sexp1)
				:for slot2 :in (slots<=obj sexp2)
				:always (rec (slot-value sexp1 slot1)
					     (slot-value sexp2 slot2))))
		     (equal sexp1 sexp2))))))
      (rec sexp1 sexp2))))

(defmacro with-integrated-output-stream((var)&body body)
  `(let((*standard-output* ,var)
	(*error-output* ,var)
	(*trace-output* ,var)
	(*query-io* (make-two-way-stream *query-io* ,var)))
     ,@body))
