(in-package :jingoh.tester)

(defun encallable(form &optional not-first-p)
  #.(doc :jingoh.tester "doc/tester/encallable.F.md")
  (typecase form
    (symbol(if not-first-p
	     `(FUNCTION ,form)
	     form))
    (function(if not-first-p
	       form
	       (or (millet:function-name form)
		   `(LAMBDA(&REST ARGS)
		      (APPLY ,form ARGS)))))
    ((cons (eql lambda) t)form)
    ((or (cons (eql function)(cons symbol null))
	 (cons (eql quote)(cons symbol null)))
     (if not-first-p
       form
       (second form)))
    (t (error'syntax-error
	 :format-control "?: ~S is not function name"
	 :format-arguments (list form)))))

(define-condition syntax-error(simple-error program-error)()
  (:documentation #.(doc :jingoh.tester "doc/tester/syntax-error.T.md")))

(defun reserved-keywords(gf)
  (loop :for method :in (closer-mop:generic-function-methods gf)
	:collect (closer-mop:eql-specializer-object(second(closer-mop:method-specializers method)))
	:into result
	:finally (return (delete-duplicates result))))

(defun canonicalize(test-form parameters)
  #.(doc :jingoh.tester "doc/tester/canonicalize.F.md")
  (labels((main(lazy)
	    (set-around (getf parameters :around)
			(if(eq lazy :does-not-exist)
			  test-form
			  (body lazy))))
	  (body(lazy)
	    (if lazy
	      `(EVAL ',test-form)
	      ;; else explicitly specified :lazy NIL.
	      (progn (eval test-form)
		     test-form)))
	  (set-around(around body)
	    (if around
	      (subst body '(CALL-BODY) around :test #'equal)
	      body)))
    (main(getf parameters :lazy :does-not-exist))))
