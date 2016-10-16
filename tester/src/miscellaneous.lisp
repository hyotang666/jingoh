(in-package :jingoh.tester)

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

(defun canonicalize(test-form parameters)
  #.(Doc :jingoh.tester "doc/canonicalize.F.md")
  (labels((MAIN(lazy)
	    (SET-AROUND (getf parameters :around)
			(if(eq lazy :does-not-exist)
			  test-form
			  (BODY lazy))))
	  (BODY(lazy)
	    (if lazy
	      `(EVAL ',test-form)
	      ;; else explicitly specified :lazy NIL.
	      (progn (eval test-form)
		     test-form)))
	  (SET-AROUND(around body)
	    (if around
	      (subst body '(CALL-BODY) around :test #'equal)
	      body)))
    (MAIN(getf parameters :lazy :does-not-exist))))
