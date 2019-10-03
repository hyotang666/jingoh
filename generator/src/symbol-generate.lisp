(in-package :jingoh.generator)

(defun symbol-generate(symbol package)
  (multiple-value-bind(s existp)(find-symbol(string symbol)package)
    (if(not existp)
      (error "Symbol ~S is not found in ~S"symbol package)
      (dolist(roll (rolls-of s))
	(funcall roll s)))))

(defun rolls-of(symbol)
  (unless(special-operator-p symbol)
    `(,@(and (millet:global-symbol-p symbol) `(,#'|variable|))
       ,@(and (symbol-macrop symbol) `(,#'|symbol-macro|))
       ,@(and (millet:type-specifier-p symbol)`(,#'|type|))
       ,@(and (fboundp symbol)`(,(function-type symbol))))))

(defun function-type(symbol)
  (if(macro-function symbol)
    #'|macro|
    (if(typep (symbol-function symbol)'standard-generic-function)
      #'|generic-function|
      #'|function|)))

(defun symbol-macrop(symbol)
  (nth-value 1 (macroexpand-1 symbol)))

(defun comentize(string)
  (format nil "~{; ~A~%~}"
	  (uiop:split-string (or string "")
			     :separator '(#\newline))))

(defun |variable|(symbol)
  (format t "(requirements-about ~A :doc-type variable)~2%~
	  ;;;; Description:~%~
	  ~A~%~
	  ;;;; Value type is ~A~%~
	  ;#? ~A :be-the ???~2%~
	  ; Initial value is ~S~2%~
	  ;;;; Affected By:~2%~
	  ;;;; Notes:~2%"
	  symbol
	  (comentize (documentation symbol 'variable))
	  (cond
	    ((when(fboundp 'cltl2:variable-information)
	       (cdr(assoc 'type (nth-value 2(cltl2:variable-information symbol))))))
	    ((boundp symbol)
	     (type-of(symbol-value symbol)))
	    (t
	      :unbound))
	  symbol
	  (if(boundp symbol)
	    (symbol-value symbol)
	    :unbound)))

(defun |symbol-macro|(symbol)
  (format t "(requirements-about ~A)~2%~
	  ;;;; Description:~%~A~~&
	  ; Expanded-form is ~S~%~
	  ;#? ~A :expanded-to ???~2%"
	  symbol
	  (comentize(documentation symbol 'function))
	  (macroexpand-1 symbol)
	  symbol))

(defun |type|(symbol)
  (let((type(type-of-type symbol)))
    (ecase type
      (:type(%type-template symbol))
      ((or :structure :condition :class)
       (class-template type symbol)))))

(defun type-of-type(symbol)
  (let((class
	 (find-class symbol nil)))
    (cond
      ((null class)
       :type)
      ((typep class 'structure-class)
       :structure)
      ((subtypep symbol 'condition)
       :condition)
      (t :class))))

(defun %type-template(symbol)
  (format t "(requirements-about ~A :doc-type type)~%~
	  ;;;; Description:~%~
	  ~A~&~
	  ;;;; Compound Type Specifier Kind:~2%~
	  ;;;; Compound Type Specifier Syntax:~2%~
	  ;;;; Compound Type Specifier Arguments:~2%~
	  ;;;; Compound Type Specifier Description:~2%"
	  symbol
	  (comentize(documentation symbol 'type))))

(defun class-template(type symbol)
  (let((class(find-class symbol)))
    (format t "(requirements-about ~A :doc-type ~A)~2%~
	    ;;;; Description:~%~
	    ~A~&~
	    ;;;; Class Precedence List: (case in ~A)~%~
	    ; ~{~(~A~)~^ ~}~2%~
	    ;;;; Effective Slots:~2%"
	    symbol
	    (if(eq :structure type)
	      type
	      :type)
	    (comentize(documentation symbol (if (eq :structure type)
					      'structure
					      'type)))
	    uiop:*implementation-type*
	    (mapcar #'class-name(closer-mop:class-precedence-list class)))
    (dolist(slot(applicables class))
      (apply #'format t
	     "; ~A [Type] ~A~%~
	     ~{~@[~{; [~A] ~{~(~S~)~^ ~}~}~%~]~}~%~
	     ~@[~A~]~&"
	     slot))
    (format t ";;;; Notes:~2%")
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
		       (handler-bind((warning #'muffle-warning))
			 (documentation slot t)))))

(defun |function|(symbol)
  (function-template symbol :function))

(defun |macro|(symbol)
  (function-template symbol :macro))

(defun |generic-function|(symbol)
  (function-template symbol :generic-function))

(defun function-template(symbol roll)
  (let((lambda-list(millet:lambda-list symbol))
       (setf-expander(setf-expander symbol))
       (notation(ensure-symbol-notation symbol)))
    (format t "(requirements-about ~A :doc-type function)~2%~
    ;;;; Description:~%~
    ~A~%~
    #+syntax~%(~A~@[ ~{~(~S~)~^ ~}~]) ; => result~2%~
    ~@[#+setf~%~S ; => new-value~2%~]~
    ~@[;;;; Argument Precedence Order:~%; ~{~(~S~)~^ ~}~2%~]~
    ~@[;;;; Method signature:~%~{#+signature~S~%~}~%~]~
    ;;;; Arguments and Values:~2%~
    ~:{; ~(~A~) := ~@[~(~A~)~]~2%~}~
    ~{;;;; ~:(~A~):~2%~}"
    notation ; requirements-about
    (comentize(documentation symbol 'function)) ; description
    notation
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
   (parse-lambda-list symbol)
    '(|affected by| side-effects notes exceptional-situations)
    )))

(defun parse-lambda-list(symbol)
  (multiple-value-bind(type _ information)(when(fboundp 'cltl2:function-information)
					    (cltl2:function-information symbol))
    (declare(ignore _))
    (let*((lambda-list
	    (millet:lambda-list symbol))
	  (lambda-vars
	    (lambda-fiddle:extract-all-lambda-vars
	      (lambda-fiddle:flatten-lambda-list lambda-list))))
      (if(or (eq :macro type)
	     (null (assoc 'ftype information)))
	(uiop:while-collecting(acc)
	  (dolist(elt lambda-vars)
	    (acc (list elt nil)))
	  (acc (list "result" nil)))
	(let*((variables
		(mapcar (lambda(symbol)
			  (intern (format nil "?~A"symbol)))
			lambda-vars))
	      (ftype
		(cdr (assoc 'ftype information)))
	      (environment
		(cl-unification:unify (mapcar (lambda(elt)
						(typecase elt
						  (symbol elt)
						  ((cons symbol *)
						   (car elt))
						  ((cons (cons keyword *) *)
						   (cadar elt))))
					      (sublis (mapcar #'cons lambda-vars
							      variables)
						      lambda-list))
				      (second ftype))))
	  (uiop:while-collecting(acc)
	    (mapc (lambda(sym var)
		    (acc (list sym
			       (cl-unification:find-variable-value var
								   environment
								   nil))))
		  lambda-vars
		  variables)
	    (let((return
		   (third ftype)))
	      (acc (etypecase return
		     ((or null (eql *))
		      '("result" nil))
		     ((cons (eql values) *)
		      (or (loop :for return :in (cdr return)
				:for num :upfrom 1
				:until (eq '&optional return)
				:collect (list (format nil "result~D" num)
					       return))
			  '("result" nil)))
		     (t
		       (list "result" return)))))))))))

(defun ensure-symbol-notation(symbol)
  (if(uiop:string-suffix-p(prin1-to-string symbol)"|")
    (format nil "|~A|"symbol)
    (symbol-name symbol)))

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
