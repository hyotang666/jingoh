(in-package :jingoh.tester)

(defun ignore-signals(type params)
  (let((condition(getf params :ignore-signals)))
    (or (eq type condition)
	(eq T condition))))

(defun function-designator-p(symbol)
  (and (fboundp symbol)
       (not (macro-function symbol))
       (not (special-operator-p symbol))))

(defun encallable(form &optional not-first-p)
  #.(Doc :jingoh.tester "doc/encallable.F.md")
  (typecase form
    (SYMBOL(if not-first-p
	     `(FUNCTION ,form)
	     form))
    (FUNCTION(if not-first-p
	       form
	       (or (millet:function-name form)
		   `(LAMBDA(&REST ARGS)
		      (APPLY ,form ARGS)))))
    ((CONS (EQL LAMBDA) T)form)
    ((OR (CONS (EQL FUNCTION)(CONS SYMBOL NULL))
	 (CONS (EQL QUOTE)(CONS SYMBOL NULL)))
     (if not-first-p
       form
       (second form)))
    (T (error'syntax-error
	 :format-control "?: ~S is not function name"
	 :format-arguments (list form)))))

(define-condition syntax-error(simple-error program-error)()
  (:documentation #.(Doc :jingoh.tester "doc/syntax-error.T.md")))

(defun reserved-keywords(gf)
  (loop :for method :in (closer-mop:generic-function-methods gf)
	:collect (closer-mop:eql-specializer-object(second(closer-mop:method-specializers method)))
	:into result
	:finally (return (delete-duplicates result))))

(deftype option-key()
  '(member :test :lazy :ignore-signals :with-restarts :stream :before :after :around :position :as))

(defun canonicalize(test-form parameters)
  #.(Doc :jingoh.tester "doc/canonicalize.F.md")
  (alexandria:when-let((as(getf parameters :as)))
    (setf test-form(trestrul:asubst *substituter* as test-form)))
  (labels((CHECK()
	    (loop :for key :in parameters :by #'cddr
		  :unless (typep key 'option-key)
		  :do (error "Unknown options key ~S in ~S"key parameters)))
	  (MAKE-BODY()
	    (SET-AROUND (let((after(getf parameters :after)))
			  (if after
			    `(UNWIND-PROTECT ,(MAKE-PRIMARY)
					     ,after)
			    (MAKE-PRIMARY)))))
	  (MAKE-PRIMARY()
	    (let((before(getf parameters :before)))
	      (if before
		`(PROGN ,before ,test-form)
		test-form)))
	  (SET-AROUND(body)
	    (let((around(getf parameters :around)))
	      (if around
		(subst body '(CALL-BODY) around :test #'equal)
		body)))
	  )
    (CHECK)
    (case(getf parameters :lazy :does-not-exist)
      (:does-not-exist (MAKE-BODY))
      ((NIL)(let((body(MAKE-BODY)))
	      (eval body)
	      body))
      (otherwise `(EVAL ',(make-body))))))

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
			       (let((pair(assoc sexp1 env :test #'eq)))
				 (if pair
				   (eq sexp2 (cdr pair))
				   (let((pair(rassoc sexp2 env :test #'eq)))
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

(defun slots<=obj(obj)
  (mapcar #'closer-mop:slot-definition-name (closer-mop:class-slots(class-of obj))))
