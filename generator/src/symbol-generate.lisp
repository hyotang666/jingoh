(in-package :jingoh.generator)

(declaim (optimize speed))

(defun symbol-generate (symbol package)
  (multiple-value-bind (s existp)
      (find-symbol (symbol-name symbol) package)
    (if (not existp)
        (error "Symbol ~S is not found in ~S" symbol package)
        (dolist (roll (rolls-of s)) (funcall (coerce roll 'function) s)))))

(defun rolls-of (symbol)
  (unless (special-operator-p symbol)
    `(,@(and (millet:global-symbol-p symbol) `(,#'|variable|))
      ,@(and (symbol-macrop symbol) `(,#'|symbol-macro|))
      ,@(and (millet:type-specifier-p symbol) `(,#'|type|))
      ,@(and (fboundp symbol) `(,(function-type symbol))))))

(defun function-type (symbol)
  (if (macro-function symbol)
      #'|macro|
      (if (typep (symbol-function symbol) 'standard-generic-function)
          #'|generic-function|
          #'|function|)))

(defun symbol-macrop (symbol) (nth-value 1 (macroexpand-1 symbol)))

(defun comentize (string)
  (format nil "~{; ~A~%~}"
          (uiop:split-string (or string "") :separator '(#\Newline))))

(defun |variable| (symbol)
  (if (constantp symbol)
      (format t "(requirements-about ~A :doc-type variable)~2%~
                 ;;;; Constant value := ~S~%~
                 ;;;; Description:~%~
                 ~A~2%"
              symbol (symbol-value symbol)
              (comentize (documentation symbol 'variable)))
      (format t "(requirements-about ~A :doc-type variable)~2%~
	         ;;;; Description:~%~
	         ~A~%~
	         ;;;; Value type is ~A~%~
	         ;#? ~A :be-the ???~2%~
	         ; Initial value is `~S`~2%~
	         ;;;; Affected By:~2%~
	         ;;;; Notes:~2%"
              symbol (comentize (documentation symbol 'variable))
              (cond
                ((when (fboundp 'cltl2:variable-information)
                   (cdr
                     (assoc 'type
                            (nth-value 2
                                       (cltl2:variable-information symbol))))))
                ((boundp symbol) (type-of (symbol-value symbol)))
                (t :unbound))
              symbol
              (if (boundp symbol)
                  (symbol-value symbol)
                  :unbound))))

(defun |symbol-macro| (symbol)
  (format t "(requirements-about ~A)~2%~
	  ;;;; Description:~%~A~~&
	  ; Expanded-form is ~S~%~
	  ;#? ~A :expanded-to ???~2%"
          symbol (comentize (documentation symbol 'function))
          (macroexpand-1 symbol) symbol))

(defun |type| (symbol)
  (let ((type (type-of-type symbol)))
    (ecase type
      (:type (%type-template symbol))
      ((or :structure :condition :class) (class-template type symbol)))))

(defun type-of-type (symbol)
  (let ((class (find-class symbol nil)))
    (cond ((null class) :type)
          ((typep class 'structure-class) :structure)
          ((subtypep symbol 'condition) :condition)
          (t :class))))

(defun %type-template (symbol)
  (let ((lambda-list
         (progn
          #+sbcl
          (millet:lambda-list (sb-int:info :type :expander symbol)))))
    (format t "(requirements-about ~A :doc-type type)~%~
	    ;;;; Description:~%~
	    ~A~&~
	    ;;;; Compound Type Specifier Kind:~%~
	    ; TODO: Choose one of below and delete others includes this line.~%~
	    ; Specializing.~%~
	    ; Abbreviating.~%~
	    ; Combining.~%~
	    ; Predicating.~2%~
	    ;;;; Compound Type Specifier Syntax:~2%~
	    ~@[#+syntax~%(~(~A~)~{ ~(~A~)~})~]~2%~
	    ;;;; Compound Type Specifier Arguments:~2%~
	    ~@[~{; ~(~A~) := ~%~}~]~%"
            symbol (comentize (documentation symbol 'type)) symbol lambda-list
            (lambda-fiddle:extract-all-lambda-vars lambda-list))))

(defun class-template (type symbol)
  (let ((class (c2mop:ensure-finalized (find-class symbol))))
    (format t "(requirements-about ~A :doc-type ~A)~2%~
	    ;;;; Description:~%~
	    ~A~&~
	    ;;;; Class Precedence List: (case in ~A)~%~
	    ; ~{~(~A~)~^ ~}~2%~
	    ;;;; Effective Slots:~2%"
            symbol
            (if (eq :structure type)
                type
                :type)
            (comentize
              (documentation symbol
                             (if (eq :structure type)
                                 'structure
                                 'type)))
            uiop:*implementation-type*
            (mapcar #'class-name (closer-mop:class-precedence-list class)))
    (dolist (slot (applicables class))
      (apply #'format t "; ~A [Type] ~A~%~
	     ~{~@[~{; [~A] ~{~(~S~)~^ ~}~}~&~]~}~&~
	     ~@[; ~A~]~&~%"
             slot))
    (format t ";;;; Notes:~2%")))

(defun applicables (class)
  (labels ((rec (classes &optional acc)
             (if (endp classes)
                 (delete-duplicates acc :from-end t :key #'car)
                 (body (car classes) (cdr classes) acc)))
           (body (class rest acc)
             (rec (append rest (closer-mop:class-direct-superclasses class))
                  (append acc (parse-class class)))))
    (rec (list class))))

(defun parse-class (class)
  (loop :for slot :in (closer-mop:class-direct-slots class)
        :for r = (closer-mop:slot-definition-readers slot)
        :for w :of-type list = (closer-mop:slot-definition-writers slot)
        :for a :of-type list
             = (remove-if-not
                 (lambda (x)
                   (declare (type symbol x))
                   (find x w
                         :key (lambda (x)
                                (when (listp x)
                                  (cadr x)))))
                 r)
        :collect (list (closer-mop:slot-definition-name slot)
                       (closer-mop:slot-definition-type slot)
                       (list
                         (let ((it (set-difference r a)))
                           (when it
                             (list :reader it)))
                         (let ((it
                                (remove-if-not
                                  (lambda (x)
                                    (when (listp x)
                                      (find x a)))
                                  w)))
                           (when it
                             (list :writer it)))
                         (when a
                           (list :accessor a)))
                       (handler-bind ((warning #'muffle-warning))
                         (documentation slot t)))))

(defun |function| (symbol) (function-template symbol :function))

(defun |macro| (symbol) (function-template symbol :macro))

(defun |generic-function| (symbol) (function-template symbol :generic-function))

(defun function-template (symbol roll)
  (let ((lambda-list
         (lambda-fiddle:remove-aux-part (millet:lambda-list symbol)))
        (setf-expander (setf-expander symbol))
        (notation (ensure-symbol-notation symbol)))
    (funcall (formatter "(requirements-about ~A :doc-type function)~2%")
             *standard-output* notation)
    (funcall (formatter ";;;; Description:~%~A~%") *standard-output*
             (comentize (documentation symbol 'function)))
    (funcall ; Syntax
             (formatter
              #.(apply #'concatenate 'string
                       (alexandria:flatten
                         (list "~<#+syntax ~@_" ; syntax.
                               (list "~:<" ; form.
                                     "~A~^ ~1I~@_" ; notation.
                                     (list "~@{" ; lambda-list
                                           (list "~(" ; downcase.
                                                 (list "~:<~^" ; Each lambda
                                                               ; vars.
                                                       "~W~^ ~1I~@_" ; var
                                                       "~@{~W~^ ~:_~}" "~:>")
                                                 "~)~^ ~_")
                                           "~}")
                                     "~:> ~_")
                               "; => result~:>~2%"))))
             *standard-output* (list (cons notation lambda-list)))
    (when setf-expander ; setf
      (funcall (formatter "#+setf~%~S ; => new-value~2%") *standard-output*
               (destructuring-bind
                   (op name)
                   (millet:function-name setf-expander)
                 (destructuring-bind
                     (new . args)
                     (millet:lambda-list setf-expander)
                   `(,op (,name ,@args) ,new)))))
    (when (eq :generic-function roll) ; argument-precedence-order
      (funcall
        (formatter ";;;; Argument Precedence Order:~%; ~{~(~S~)~^ ~}~2%")
        *standard-output*
        (closer-mop:generic-function-argument-precedence-order
          (symbol-function symbol))))
    (when (eq :generic-function roll) ; Method Signature
      (funcall (formatter ";;;; Method signature:~%~{#+signature~S~%~}~%")
               *standard-output* (specialized-lambda-lists symbol)))
    ;; Lambda-list
    (funcall
      (formatter
       ";;;; Arguments and Values:~2%~:{; ~(~A~) := ~@[~(~A~)~]~2%~}")
      *standard-output* (parse-lambda-list symbol))
    (funcall (formatter "~{;;;; ~:(~A~):~2%~}") *standard-output*
             '(|affected by| side-effects notes exceptional-situations))))

(defun parse-lambda-list (symbol)
  (multiple-value-bind (type _ information)
      (when (fboundp 'cltl2:function-information)
        (cltl2:function-information symbol))
    (declare (ignore _))
    (let* ((lambda-list
            (lambda-fiddle:remove-aux-part (millet:lambda-list symbol)))
           (lambda-vars (lambda-fiddle:extract-lambda-vars lambda-list)))
      (if (or (eq :macro type)
              (member (assoc 'ftype information) '(nil (ftype . function))
                      :test #'equal))
          (uiop:while-collecting (acc)
            (dolist (elt lambda-vars) (acc (list elt nil)))
            (acc (list "result" nil)))
          (let* ((variables
                  (mapcar (lambda (symbol) (intern (format nil "?~A" symbol)))
                          lambda-vars))
                 (ftype (cdr (assoc 'ftype information)))
                 (environment
                  (cl-unification:unify
                    (mapcar
                      (lambda (elt)
                        (typecase elt
                          (symbol elt)
                          ((cons symbol *) (car elt))
                          ((cons (cons keyword *) *) (cadar elt))))
                      (sublis (mapcar #'cons lambda-vars variables)
                              lambda-list))
                    (let ((type (second ftype)))
                      (if (eq '* type)
                          '_
                          type)))))
            (uiop:while-collecting (acc)
              (mapc
                (lambda (sym var)
                  (acc
                   (list sym
                         (let ((value
                                (cl-unification:find-variable-value var
                                                                    environment
                                                                    nil)))
                           (when value
                             (write-to-string
                               (if (typep value '(cons keyword *))
                                   (cadr value)
                                   value)
                               :pretty nil))))))
                lambda-vars variables)
              (let ((return (third ftype)))
                (mapc #'acc
                      (typecase return
                        ((or null (eql *)) '(("result" nil)))
                        ((cons (eql values) *) (parse-values return))
                        (otherwise (list (list "result" return))))))))))))

(defun parse-values (values)
  (labels ((rec (spec* &optional optionalp (num 1) acc)
             (if (endp spec*)
                 (do-return acc)
                 (body (car spec*) (cdr spec*) optionalp num acc)))
           (do-return (acc)
             (case (length acc)
               (0 `(("result" nil)))
               (1
                (destructuring-bind
                    ((k v))
                    acc
                  (if (equal "result 1" k)
                      `(("result" ,v))
                      acc)))
               (otherwise (nreverse acc))))
           (body (spec rest optionalp num acc)
             (case spec
               (&optional (rec rest "optional" 1 acc))
               (&rest
                (rec (cdr rest) optionalp (1+ num)
                     (cons
                       (list "rest values"
                             (write-to-string (car rest) :pretty nil))
                       acc)))
               (&allow-other-keys (do-return acc))
               (otherwise
                (rec rest optionalp (1+ num)
                     (cons
                       (list (format nil "~@[~A ~]result ~D" optionalp num)
                             (write-to-string spec :pretty nil))
                       acc))))))
    (declare
      (ftype (function
              (* list (or null string) (mod #.most-positive-fixnum) list)
              (values list &optional))
             body)
      (ftype (function (list) (values list &optional)) do-return))
    (rec (cdr values))))

(defun ensure-symbol-notation (symbol)
  (if (uiop:string-suffix-p (prin1-to-string symbol) "|")
      (format nil "|~A|" symbol)
      (symbol-name symbol)))

(defun setf-expander (symbol) (ignore-errors (fdefinition `(setf ,symbol))))

(defun specialized-lambda-lists (symbol)
  (loop :for method
             :in (closer-mop:generic-function-methods (symbol-function symbol))
        :for qualifiers := (method-qualifiers method)
        :collect `(,symbol ,@qualifiers ,@(specialized-lambda-list method))))

(defun specialized-lambda-list (method)
  (labels ((rec (specializers lambda-lists &optional acc)
             (if (endp specializers)
                 (nreconc acc lambda-lists)
                 (rec (cdr specializers) (cdr lambda-lists)
                      (push
                       `(,(car lambda-lists) ,(specializer (car specializers)))
                       acc))))
           (specializer (specializer)
             (if (string= :eql-specializer (type-of specializer))
                 (list 'eql (closer-mop:eql-specializer-object specializer))
                 (class-name specializer))))
    (rec (closer-mop:method-specializers method)
         (closer-mop:method-lambda-list method))))