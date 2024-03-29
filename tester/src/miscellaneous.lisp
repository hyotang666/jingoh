(in-package :jingoh.tester)

(declaim (optimize speed))

(defun ignore-signals (type params)
  (let* ((sentinel '#:not-ignore-signals)
         (condition (getf params :ignore-signals sentinel)))
    (unless (eq sentinel condition)
      (or (subtypep condition type) (eq t condition) (eq nil condition)))))

(defun function-designator-p (symbol)
  (and (symbolp symbol)
       (fboundp symbol)
       (not (macro-function symbol))
       (not (special-operator-p symbol))))

(defun encallable (form &optional not-first-p)
  (typecase form
    (symbol
     (if not-first-p
         `#',form
         form))
    ((cons (eql lambda) t) form)
    ((or (cons (eql function) (cons symbol null))
         (cons (eql quote) (cons symbol null)))
     (if not-first-p
         `#',(cadr form)
         (second form)))
    (t
     (error 'syntax-error
            :format-control "?: ~S is not function name"
            :format-arguments (list form)))))

(define-condition syntax-error (simple-error program-error) ())

(defun reserved-keywords (gf)
  (loop :for method :in (c2mop:generic-function-methods gf)
        :collect (c2mop:eql-specializer-object
                   (second (c2mop:method-specializers method)))
          :into result
        :finally (return (delete-duplicates result))))

(macrolet ((defs (&body keys)
             `(progn
               ,@(mapcar (lambda (key) `(add-new-option-key ,key)) keys))))
  (defs :test :lazy :ignore-signals :with-restarts :stream :before :after
    :around :line :timeout :comment))

(defun canonicalize (test-form parameters)
  (setf test-form (copy-cons test-form))
  (labels ((check ()
             (loop :for key :in parameters :by #'cddr
                   :do (handler-case (find-option-key key)
                         (error (c)
                           (error
                             (concatenate 'string (princ-to-string c)
                                          " in ~S ~:@_Hint: ~S?")
                             parameters
                             (fuzzy-match:fuzzy-match (symbol-name key)
                                                      (list-all-option-keys)))))))
           (make-body (test-form)
             (set-around
               (let ((after (getf parameters :after)))
                 (if after
                     `(unwind-protect ,(make-primary test-form) ,after)
                     (make-primary test-form)))))
           (make-primary (test-form)
             (let ((before (getf parameters :before))
                   (ignore-output-p
                    (null (getf parameters :stream '#:not-specified)))
                   (time (getf parameters :timeout 1)))
               (if before
                   (if ignore-output-p
                       `(let ((*standard-output* (make-broadcast-stream)))
                          (with-integrated-output-stream (*standard-output*)
                            ,before
                            ,(may-make-timeout-form time test-form)))
                       `(progn
                         ,before
                         ,(may-make-timeout-form time test-form)))
                   (if ignore-output-p
                       `(let ((*standard-output* (make-broadcast-stream)))
                          (with-integrated-output-stream (*standard-output*)
                            ,(may-make-timeout-form time test-form)))
                       (may-make-timeout-form time test-form)))))
           (may-make-timeout-form (time test-form)
             (if bt:*supports-threads-p*
                 `(bt:with-timeout (,time)
                    ,test-form)
                 test-form))
           (set-around (body)
             (let ((around (getf parameters :around)))
               (if around
                   (subst body '(call-body) around :test #'equal)
                   body))))
    (check)
    (case (getf parameters :lazy :does-not-exist)
      (:does-not-exist (make-body test-form))
      ((nil)
       `(uiop:call-with-muffled-conditions (lambda () ,(make-body test-form))
                                           uiop:*usual-uninteresting-conditions*))
      (otherwise (make-body `(eval (macroexpand ',test-form)))))))

(defun slots<=obj (obj)
  (mapcar #'c2mop:slot-definition-name (c2mop:class-slots (class-of obj))))

(defun sexp= (sexp1 sexp2)
  (let (env)
    (labels ((rec (sexp1 sexp2)
               (typecase sexp1
                 (cons
                  (and (consp sexp2)
                       (rec (car sexp1) (car sexp2))
                       (rec (cdr sexp1) (cdr sexp2))))
                 ((or boolean keyword) (eq sexp1 sexp2))
                 (symbol
                  (and (symbolp sexp2)
                       (if (symbol-package sexp1)
                           (eq sexp1 sexp2)
                           (let ((pair (assoc sexp1 env :test #'eq)))
                             (if pair
                                 (eq sexp2 (cdr pair))
                                 (let ((pair (rassoc sexp2 env :test #'eq)))
                                   (unless pair
                                     (push (cons sexp1 sexp2) env)
                                     t)))))))
                 ((or string number character bit-vector pathname)
                  (equal sexp1 sexp2))
                 (vector
                  (and (vectorp sexp2)
                       (locally
                        #+sbcl
                        (declare
                          (sb-ext:muffle-conditions sb-ext:compiler-note))
                        (loop :for elt1 :across sexp1
                              :for elt2 :across sexp2
                              :always (rec elt1 elt2)))))
                 (array
                  (and (arrayp sexp2)
                       (equal (array-dimensions sexp1)
                              (array-dimensions sexp2))
                       (locally
                        #+sbcl
                        (declare
                          (sb-ext:muffle-conditions sb-ext:compiler-note))
                        (dotimes (i (array-total-size sexp1) t)
                          (unless (rec (row-major-aref sexp1 i)
                                       (row-major-aref sexp2 i))
                            (return nil))))))
                 (t
                  (if (typep (class-of sexp1) 'structure-class)
                      (and (eq (type-of sexp1) (type-of sexp2))
                           (loop :for slot1 :in (slots<=obj sexp1)
                                 :for slot2 :in (slots<=obj sexp2)
                                 :always (rec (slot-value sexp1 slot1)
                                              (slot-value sexp2 slot2))))
                      (equal sexp1 sexp2))))))
      (rec sexp1 sexp2))))

(defmacro with-integrated-output-stream ((var) &body body)
  `(let ((*standard-output* ,var)
         (*error-output* ,var)
         (*trace-output* ,var)
         (*debug-io* (make-two-way-stream *debug-io* ,var))
         (*query-io* (make-two-way-stream *query-io* ,var)))
     ,@body))

(defun copy-cons (cons)
  (let ((seen (make-hash-table :test #'eq)))
    (labels ((rec (thing)
               (if (atom thing)
                   thing
                   (or (gethash thing seen)
                       (let ((new (cons nil nil)))
                         (setf (gethash thing seen) new)
                         (rplaca new (rec (car thing)))
                         (rplacd new (rec (cdr thing)))
                         new)))))
      (rec cons))))
