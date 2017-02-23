(defpackage :jingoh.generator(:use :cl)
  (:export
    #:generate
    ))
(in-package :jingoh.generator)

(defgeneric generate(arg &key))

(defmethod generate ((system asdf:system) &key)
  (let(forms)
    (labels((LOAD-DEPENDENCIES(system)
	      (mapc #'asdf:load-system(asdf:system-depends-on system)))
	    (HOOK(expander form env)
	      (when(DEFPACKAGEP form)
		(push form forms))
	      (funcall expander form env))
	    (DEFPACKAGEP(form)
	      (typep form '(CONS(EQL DEFPACKAGE)T)))
	    )
      (LOAD-DEPENDENCIES system)
      (let((*macroexpand-hook* #'HOOK))
	(asdf:load-system system :force t))
      (let((*default-pathname-defaults*(spec-directory system)))
	(generate-asd system forms)
	(map nil #'generate forms)))))

(defun spec-directory(system)
  (uiop:subpathname (asdf:system-source-directory (asdf:find-system system))
		    "spec/"))

(defun generate-asd(system forms)
  (ensure-directories-exist *default-pathname-defaults*)
  (with-open-file(*standard-output* (format nil "~A~A.test.asd"
					    *default-pathname-defaults*
					    (asdf:coerce-name system))
				    :direction :output
				    :if-exists :supersede
				    :if-does-not-exist :create)
    (%generate-asd system forms)))

(defun %generate-asd(system forms)
  (labels((component(form)
	    `(:file ,(string-downcase(second form)))))
    (let((*package* (find-package :asdf)))
      (format t "; vim: ft=lisp et~%~
	      (in-package :asdf)~%~
	      ~(~S~)"
	      `(asdf:defsystem ,(intern (format nil "~:@(~A~).TEST"
						(asdf:coerce-name system))
					:keyword)
			  :depends-on (:jingoh ,(asdf:coerce-name system))
			  :components ,(mapcar #'component forms)
			  :perform (asdf:test-op(asdf::o asdf::c)
				     (uiop:symbol-call :jingoh :verify)))))))

(defmethod generate((form list) &key)
  (assert(typep form '(CONS (EQL DEFPACKAGE) T)))
  (macrolet((expand(existp)
	      `(WITH-OPEN-FILE(*STANDARD-OUTPUT* PATH
				:DIRECTION :OUTPUT
				,@(if existp
				    `(:IF-EXISTS :APPEND)
				    `(:IF-DOES-NOT-EXIST :CREATE)))
		 ,@(unless existp
		     `((GENERATE-HEADER(SECOND FORM))))
		 (DOLIST(SYMBOL (EXPORTS FORM))
		   (SYMBOL-GENERATE SYMBOL (SECOND FORM))))))
    (labels((exports(form)
	      (loop :for elt :in form
		    :when (typep elt '(CONS(EQL :EXPORT)T))
		    :append (cdr elt))))
      (let((path(format nil "~A~(~A~).lisp"
			*default-pathname-defaults*
			(second form)))
	   (*package*(find-package(second form))))
	(if (probe-file path)
	  (expand t)
	  (expand nil))))))

(defun generate-header(package-name)
  (let((spec-name(intern (format nil "~A.SPEC"package-name)
			 :keyword)))
    (format t "~(~S~)~%~
	    (in-package ~(~S~))~%~
	    (setup ~(~:*~S~))~2%"
	    `(defpackage ,spec-name (:use :cl :jingoh ,package-name))
	    spec-name
	    )))

(defmethod generate ((symbol symbol) &key system)
  (if(keywordp symbol)
    (generate(asdf:find-system symbol))
    (let*((*package* (symbol-package symbol))
	  (package-name(package-name *package*))
	  (system(asdf:find-system (or system (string-downcase package-name))))
	  (*default-pathname-defaults*(spec-directory system))
	  (forms `((defpackage ,package-name
		     (:export ,symbol))))
	  (path(format nil "~A.test.asd"
		       *default-pathname-defaults*)))
      (macrolet((expand(existsp)
		  `(PROGN ,@(unless existsp
			      `((GENERATE-ASD SYSTEM FORMS)))
			  (MAP NIL #'GENERATE FORMS))))
	(if(probe-file path)
	  (expand T)
	  (expand nil))))))

(defun symbol-generate(symbol package)
  (let((symbol(find-symbol (string symbol)package)))
    (if(null symbol)
      (error "Symbol ~S is not found in ~S"symbol package)
      (dolist(roll (rolls-of symbol))
	(funcall roll symbol)))))

(defun rolls-of(symbol)
  `(,@(and (millet:global-symbol-p symbol) `(,#'|variable|))
     ,@(and (symbol-macrop symbol) `(,#'|symbol-macro|))
     ,@(and (millet:type-specifier-p symbol)`(,#'|type|))
     ,@(and (fboundp symbol)`(,(function-type symbol)))))

(defun function-type(symbol)
  (if(macro-function symbol)
    #'|macro|
    (if(typep (symbol-function symbol)'standard-generic-function)
      #'|generic-function|
      #'|function|)))

(defun symbol-macrop(symbol)
  (nth-value 1 (macroexpand-1 symbol)))

(defun |variable|(symbol)
  (format t "(requirements-about ~A)~2%~
	  #| [Variable] ~:*~A~%~
	  ~@[~A~]~
	  ~&|#~2%~
	  ;; Value type is ~A~%~
	  #? ~A :be-the ???~2%~
	  ;; Initial value is ~S~2%~
	  #| Affected By: |#~2%~
	  #| Notes: |#~2%"
	  symbol
	  (documentation symbol 'variable)
	  (if(boundp symbol)
	    (type-of(symbol-value symbol))
	    :unbound)
	  symbol
	  (if(boundp symbol)
	    (symbol-value symbol)
	    :unbound)))

(defun |symbol-macro|(symbol)
  (format t "(requirements-about ~A)~2%~
	  #| [Symbol-macro] ~:*~S~%~
	  ~@[~A~]~
	  ~&|#~2%~
	  ;; Expanded-form is ~S~%~
	  #? ~A :expanded-to ???~2%"
	  symbol
	  (documentation symbol 'function)
	  (macroexpand-1 symbol)
	  symbol))

(defun |type|(symbol)
  (let((type(type-of-type symbol)))
    (ecase type
      (:type(%type-template symbol))
      ((or :structure :condition :class)
       (class-template type symbol)))))

(defun type-of-type(symbol)
  (let((instance(ignore-errors(make-instance symbol))))
    (if(not instance)
      :type
      (if(typep (class-of instance)'structure-class)
	:structure
	(if(typep instance 'condition)
	  :condition
	  :class)))))

(defun %type-template(symbol)
  (format t "#| [Type] ~A~%~
	  ~@[~A~2%~]~
	  |#~2%~
	  ;; Compound Type Specifier Kind:~2%~
	  ;; Compound Type Specifier Syntax:~2%~
	  ;; Compound Type Specifier Arguments:~2%~
	  ;; Compound Type Specifier Description:~2%"
	  symbol
	  (documentation symbol 'type)))

(defun class-template(type symbol)
  (let((class(find-class symbol)))
    (format t "(requirements-about ~A)~2%~
	    #|~
	    [~:(~A~)] ~A~%~
	    ~@[~A~]~
	    ~&|#~2%~
	    ;; Class Precedence List: (case in ~A)~%~
	    ;; ~{~(~A~)~^ ~}~2%~
	    ;; Effective Slots:~2%"
	    symbol
	    type
	    symbol
	    (documentation symbol 'type)
	    uiop:*implementation-type*
	    (mapcar #'class-name(closer-mop:class-precedence-list class)))
    (dolist(slot(applicables class))
      (apply #'format t
	     ";; ~A [Type] ~A~%~
	     ~{~@[~{;; [~A] ~{~(~S~)~^ ~}~}~%~]~}~%~
	     ~@[~A~]~&"
	     slot))
    (format t "#| Notes: |#~2%")
    ))

(defun applicables(class)
  (labels((REC(classes &optional acc)
	    (if(endp classes)
	      acc
	      (BODY(car classes)(cdr classes)acc)))
	  (BODY(class rest acc)
	    (REC (append rest (closer-mop:class-direct-superclasses class))
		 (append acc(parse-class class))))
	  )
    (REC(list class))))

(defun parse-class(class)
  (loop :for slot :in (closer-mop:class-direct-slots class)
	:for r = (closer-mop:slot-definition-readers slot)
	:for w = (closer-mop:slot-definition-writers slot)
	:for a = (remove-if(complement(lambda(x)
					(find x w :key (lambda(x)
							 (when(listp x)
							   (cadr x))))))
		   r)
	:collect (list (closer-mop:slot-definition-name slot)
		       (closer-mop:slot-definition-type slot)
		       
		       (list (let((it(set-difference r a)))
			       (when it (list :reader it)))
			     (let((it(remove-if(complement(lambda(x)
							    (when(listp x)
							      (find x a))))
				       w)))
			       (when it (list :writer it)))
			     (when a (list :accessor a)))
		       (documentation slot t))))

(defun |function|(symbol)
  (function-template symbol :function))

(defun |macro|(symbol)
  (function-template symbol :macro))

(defun |generic-function|(symbol)
  (function-template symbol :generic-function))

(defun function-template(symbol roll)
  (let((lambda-list(millet:lambda-list symbol))
       (setf-expander(setf-expander symbol)))
    (format t "(requirements-about ~A)~2%~
    ;;;; [~:(~A~)] ~A~2%~
    #| Description: ~@[~%~A ~]|#~2%~
    #+syntax~%(~A~@[ ~{~(~S~)~^ ~}~]) ; => result~2%~
    ~@[~S ; => new-value~2%~]~
    ~@[#| Argument Precedence Order:~%~{~(~S~)~^ ~}~%|#~2%~]~
    ~@[#| Method signature:~%~{~S~%~}|#~2%~]~
    ;;; Arguments and Values:~2%~
    ~{#| ~(~A~) := |#~2%~}#| result := |#~2%~
    ~{#| ~:(~A~): |#~2%~}"
    symbol ; requirements-about
    (if setf-expander ; roll
      :accessor
      roll)
    symbol ; name
    (documentation symbol 'function) ; description
    symbol
    lambda-list ; lambda-list
    (when setf-expander ; setf
      (destructuring-bind(op name)(millet:function-name setf-expander)
	(destructuring-bind(new . args)(millet:lambda-list setf-expander)
	  `(,op(,name ,@args),new))))
    (when(eq :generic-function roll) ; argument-precedence-order
      (closer-mop:generic-function-argument-precedence-order(symbol-function symbol)))
    (when(eq :generic-function roll) ; Method Signature
      (specialized-lambda-lists symbol))
    ;; Arguments and Values
    (lambda-list:vars<=lambda-list lambda-list :as (if(eq :generic-function roll)
						     :method
						     roll))
    '(|affected by| side-effects notes exceptional-situations)
    )))

(defun setf-expander(symbol)
  (ignore-errors(fdefinition `(SETF ,symbol))))

(defun specialized-lambda-lists(symbol)
  (loop :for method :in (closer-mop:generic-function-methods(symbol-function symbol))
	:collect (cons symbol (specialized-lambda-list method))))

(defun specialized-lambda-list(method)
  (labels((REC(specializers lambda-lists &optional acc)
	    (if(endp specializers)
	      (nreconc acc lambda-lists)
	      (REC (cdr specializers)
		   (cdr lambda-lists)
		   (push `(,(car lambda-lists)
			    ,(SPECIALIZER(car specializers)))
			 acc))))
	  (SPECIALIZER(specializer)
	    (if(string= :eql-specializer(type-of specializer))
	      (list 'eql (closer-mop:eql-specializer-object specializer))
	      (class-name specializer))))
    (REC (closer-mop:method-specializers method)
	 (closer-mop:method-lambda-list method))))

(defvar +cl-special-symbols+ (loop :for symbol :being :each :external-symbol :of :cl
				  :when (millet:special-symbol-p symbol)
				  :collect symbol))
(defun special-symbols(symbols)
  (loop :for symbol :in symbols
	:when (millet:special-symbol-p symbol)
	:collect symbol :into result
	:finally (return(append +cl-special-symbols+ result))))