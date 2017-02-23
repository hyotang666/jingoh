(in-package :jingoh.tester)

(defstruct issue form expected actual position)

(defstruct(test-issue(:include issue))
  test)

(defstruct(condition-issue (:include issue))
  message)

(defstruct(error-was-signaled (:include condition-issue)))
(defstruct(warning-was-signaled (:include condition-issue)))
(defstruct(debugger-was-invoked (:include condition-issue)))
(defstruct(unmatch-condition (:include condition-issue)))

(defstruct(unexpected-success (:include issue)))
(defstruct(unexpected-output (:include issue)))
(defstruct(issue-of-multiple-values (:include issue)))
(defstruct(missing-restarts(:include issue)))
(defstruct(unsatisfied-clause (:include issue))
  args)

(defstruct(wrong-format (:include test-issue)))

#+(or sbcl)
(defmethod make-instance :around ((type structure-class) &rest args)
  (loop :with instance = (closer-mop:class-prototype type)
	:for slot :in (closer-mop:class-slots type)
	:for name = (closer-mop:slot-definition-name slot)
	:do(setf (slot-value instance name)
		 (or (getf args (intern(symbol-name name):keyword))
		     (funcall(closer-mop:slot-definition-initfunction slot))))
	:finally (return instance)))

(defparameter *print-vivid* T)

(defmethod print-object((issue issue)stream)
  (if (null *print-vivid*)
    (call-next-method)
    (let((string (with-output-to-string(stream)
		   (let((i(copy-structure issue)))
		     (when(and (not (typep issue '(condition-issue unexpected-success unexpected-output missing-restarts)))
			       (or (typep (issue-expected issue)
					  '(or sequence pathname array))
				   (typep (class-of(issue-expected issue))
					  'structure-class)))
		       (setf (issue-actual i)
			     (mismatch-sexp (issue-actual issue)
					    (issue-expected issue))))
		     (call-next-method i stream)))))
      (princ(regex-replace issue string)stream))))

(defun regex-replace(issue string)
  (labels((REC(slots acc)
	    (if(endp slots)
	      (DO-RETURN acc)
	      (BODY (symbol-name(c2mop:slot-definition-name(car slots)))
		    (cdr slots)
		    :yellow
		    acc)))
	  (BODY(slot-name rest color &optional acc)
	    (destructuring-bind(pre post)(ppcre:split slot-name string :limit 2)
	      (setf string post)
	      (REC rest (list* (uiop:symbol-call :cl-ansi-text color slot-name)
			       pre acc))))
	  (DO-RETURN(acc)
	    (apply #'concatenate 'string (nreverse(push string acc))))
	    )
    (BODY (symbol-name(type-of issue))
	  (c2mop:class-slots(class-of issue))
	  :red)))

(defstruct(diff(:constructor markup (object)))
  object)
(defstruct(string-diff(:include diff)
	    (:constructor markup-string (object origin)))
  origin)
(defmethod print-object((object diff)*standard-output*)
  (cl-ansi-text:with-color(:red)
    (prin1(diff-object object))))

(defvar *color-hook* #'cl-ansi-text:red)

(defmethod print-object((object string-diff)*standard-output*)
  (let*((pre-start 0)
	(pre-end(or (mismatch (string-diff-origin object)
			      (string-diff-object object))
		    0))
	(pre-string (subseq (string-diff-object object)pre-start pre-end))
	(rev-origin(reverse(string-diff-origin object)))
	(rev-object(reverse(string-diff-object object)))
	(rev-end(mismatch rev-origin rev-object))
	(post-start(- (length(string-diff-object object))
		      rev-end))
	(post-string(nreverse(subseq rev-object 0 rev-end))))
    (prin1 (concatenate 'string
			pre-string
			(funcall *color-hook* (subseq (string-diff-object object)
						      pre-end post-start))
			(cond
			  ;; too much short
			  ((= pre-end (length (string-diff-object object)))
			   (funcall *color-hook* ":NULL"))
			  ;; too much long.
			  ((= pre-end (length (string-diff-origin object)))
			   (funcall *color-hook* post-string))
			  ((= pre-end post-start) ; lack of middle.
			   (concatenate 'string (funcall *color-hook* ":NULL")
					post-string))
			  (t post-string))))))

(defun mismatch-sexp(actual expected)
  (let(env)
    (labels((rec(actual expected)
	      (typecase expected
		(cons (if (atom actual)
			(markup actual)
			(cons (rec (car actual)(car expected))
			      (rec (cdr actual)(cdr expected)))))
		((or boolean keyword)
		 (if(eq expected actual)
		   actual
		   (markup actual)))
		(symbol (if(not (symbolp actual))
			  (markup actual)
			  (if(symbol-package actual)
			    (if(eq expected actual)
			      actual
			      (markup actual))
			    ;; actual is uninterned symbol.
			    (let((pair(assoc actual env :test #'eq)))
			      (if pair ; seen.
				(if(eq expected (cdr pair))
				  actual
				  (markup actual))
				; unseen.
				(let((pair(rassoc expected env :test #'eq)))
				  (if pair ; seen.
				    (markup actual)
				    (progn (push (cons actual expected) env)
					   actual))))))))
		(string (if (not(stringp actual))
			  (markup actual)
			  (if(string= expected actual)
			    actual
			    (markup-string actual expected))))
		(pathname (if (not(pathnamep actual))
			    (markup actual)
			    (if (equal expected actual)
			      actual
			      (markup-string (namestring actual)
					     (namestring expected)))))
		((or number character bit-vector)
		 (if(equal expected actual)
		   actual
		   (markup actual)))
		(vector (if (not(vectorp actual))
			  (markup actual)
			  (do*((i 0 (1+ i))
			       (a-p (array-in-bounds-p expected i)(array-in-bounds-p expected i))
			       (b-p (array-in-bounds-p actual i)(array-in-bounds-p actual i))
			       (acc))
			    ((or (and (not a-p)(not b-p)) ; same length
				 (and (not a-p)b-p) ; actual is longer
				 (and a-p(not b-p))) ; actual is shorter 
			     (cond
			       ((and (not a-p)(not b-p)) ; same length
				(coerce (nreverse acc)'vector))
			       ((and (not a-p)b-p) ; actual is longer
				(concatenate 'vector (nreverse acc)
					     (map 'vector #'markup(subseq actual i))))
			       ((and a-p (not b-p)) ; actual is shorter
				(coerce (nreverse (cons :null acc))'vector))))
			    (push (rec (aref actual i)(aref expected i))
				  acc))))
		(array (if (not(arrayp actual))
			 (markup actual)
			 (if(equal (array-dimensions expected)
				   (array-dimensions actual))
			   (markup (list :different-dimensions
					 :expected (array-dimensions expected)
					 :actual (array-dimensions actual)
					 actual))
			   (let((a(make-array(array-dimensions actual))))
			     (dotimes(i (array-total-size expected)a)
			       (setf (row-major-aref a i)
				     (rec (row-major-aref actual i)
					  (row-major-aref expected i))))))))
		(t (if (not(typep(class-of expected)'structure-class))
		     (if(equal expected actual)
		       actual
		       (markup actual))
		     (if (not(eq (type-of expected)(type-of actual)))
		       (markup actual)
		       (loop :with object = (copy-structure actual)
			     :for slot1 :in (slots<=obj expected)
			     :for slot2 :in (slots<=obj actual)
			     :do (setf (slot-value object slot2)
				       (rec (slot-value actual slot2)
					    (slot-value expected slot1)))))))))
	    )
      (rec actual expected))))

(defun slots<=obj(obj)
  (mapcar #'closer-mop:slot-definition-name (closer-mop:class-slots(class-of obj))))
