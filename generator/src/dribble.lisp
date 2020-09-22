(in-package #:jingoh.generator)

(defvar *spec-output* *standard-output*)

(defvar *spec-append-hook* 'funcall)

(defun dribble (system &optional package)
  (let* ((*package*
          (or (when package
                (or (find-package package)
                    (error "Package missing: ~S" package)))
              *package*))
         (*default-pathname-defaults*
          (path-of (string-downcase (package-name *package*)) "lisp"
                   (spec-directory system)))
         (*spec-append-hook* 'spec-appender))
    (repl)))

(defun spec-appender (appender)
  (with-open-file (*spec-output* *default-pathname-defaults* :direction :output
                   :if-exists :append)
    (funcall appender)))

(defun repl ()
  (catch 'quit
    (loop (restart-case (multiple-value-call #'dribble-print
                          (funcall *spec-append-hook*
                                   (lambda () (dribble-eval (dribble-read)))))
            (dribble () :report "Return to dribble.")))))

(defun dribble-read (&optional (*standard-input* *query-io*))
  (let ((*standard-output* *query-io*))
    (format t "~%DRIBBLE> ")
    (force-output)
    (read)))

(defgeneric spec-of (dispatcher form result))

(defmethod spec-of :around (a b c)
  (declare (ignore a b c))
  (the (values null &optional) (call-next-method)))

(defvar *special-commands* (make-hash-table))

(defun get-special-command (form) (cdr (gethash form *special-commands*)))

(defmacro define-special-command (command description &body body)
  `(progn
    (setf (gethash ',command *special-commands*)
            (cons ,description (lambda () ,@body)))
    ',command))

(define-special-command :q
    "Quit dribble repl, returning to top level."
  (throw 'quit (values)))

(define-special-command :g
    "Generate specified symbol templates."
  (let ((symbol (prompt-for:prompt-for 'symbol "~&>> "))
        (*standard-output* *spec-output*))
    (fresh-line)
    (symbol-generate symbol (symbol-package symbol))))

(defun print-descriptions ()
  (let ((max
         (loop :for key :being :each :hash-key :of *special-commands*
               :maximize (length (prin1-to-string key)))))
    (loop :for (description . nil) :being :each :hash-value :of
               *special-commands* :using (:hash-key command)
          :do (format t "~%[~VS] :~A" max command description))))

(defun dribble-eval (form)
  (when (and (listp form) (string= 'dribble (car form)))
    (throw 'quit (values)))
  (when (or (and (typep form 'symbol)
                 (string= "?" form)
                 (progn (print-descriptions) t))
            (and (get-special-command form)
                 (progn (funcall (get-special-command form)) t)))
    (return-from dribble-eval (values)))
  (let* ((condition)
         (result)
         (output
          (restart-case (handler-bind ((condition
                                        (lambda (c) (setq condition c))))
                          (with-output-to-string (s)
                            (let ((*standard-output*
                                   (make-broadcast-stream s *query-io*)))
                              (setq result (multiple-value-list (eval form)))
                              (force-output))))
            (append-spec ()
                :report "This is expected behavior, returning to dribble."
              (format *spec-output* "~%#?~S :signals ~S" form
                      (type-of condition))
              (return-from dribble-eval (values))))))
    (shiftf +++ ++ + form)
    (shiftf *** ** * (car result))
    (shiftf /// // / result)
    (cond ((find form '(+++ ++ + *** ** * /// // /) :test #'equal)) ; do nothing
          (t (spec-of :condition form condition) (spec-of :output form output)
           (if (cdr result) ; multiple-value.
               (if (typep form '(cons (eql macroexpand-1) *))
                   (spec-of :expansion form (car result))
                   (spec-of :values form result))
               (if (unreadable-objectp (car result))
                   (spec-of :unreadable form (car result))
                   (spec-of :default form (list* condition output result))))))
    (values-list result)))

(define-condition unexpected-behavior (error) ())

;;; SPEC-OF methods

(defmethod spec-of ((d (eql :condition)) form condition)
  (when (and (typep condition 'warning)
             (y-or-n-p "Expected signals? ~S" condition))
    (format *spec-output* "~@<~%#?~S ~_:signals ~S~:>" form
            (type-of condition))
    (force-output *spec-output*)))

(defmethod spec-of ((d (eql :output)) form output)
  (unless (equal "" output)
    (format *spec-output* "~@<~%#?~S ~_:outputs ~S~:>" form
            (if (y-or-n-p "Expected output?")
                output
                (restart-case (error 'unexpected-behavior)
                  (use-value (expected)
                      :report "Specify expected output"
                      :interactive (lambda () (list (read-expected)))
                    expected)
                  (ignore ()
                      :report "Ignore about outputs."
                    (return-from spec-of nil)))))
    (force-output *spec-output*)))

(defmethod spec-of ((d (eql :expansion)) form result)
  (format *spec-output* "~@<~%#?~S ~_:expanded-to ~S~:>" (cadr form)
          (if (y-or-n-p "~S~%Expected expansion?" result)
              result
              (restart-case (error 'unexpected-behavior)
                (use-value (expected)
                    :report "Specify expected expression."
                    :interactive (lambda ()
                                   (list
                                     (prompt-for:prompt-for t
                                                            "Input expected form. >> ")))
                  expected))))
  (force-output *spec-output*))

(defmethod spec-of ((d (eql :values)) form result)
  (if (some #'unreadable-objectp result)
      (if (y-or-n-p "~{~S~%~}Expected values?" result)
          (format *spec-output* "~%#?~S~%:multiple-value-satisfies~%~S" form
                  `(lambda
                       ,(loop :for i :upfrom 1 :to (length result)
                              :collect (intern (format nil "RESULT~D" i)))
                     :todo))
          (error 'unexpected-behavior))
      (format *spec-output* "~%#?~S~%:values ~S" form
              (if (y-or-n-p "~{~S~%~}Expected values?" result)
                  result
                  (restart-case (error 'unexpected-behavior)
                    (use-value (expected)
                        :report "Specify expected values as list."
                        :interactive (lambda ()
                                       (list
                                         (prompt-for:prompt-for 'list
                                                                "Input expected values. >> ")))
                      expected)))))
  (force-output *spec-output*))

(defmethod spec-of ((d (eql :unreadable)) form result)
  (format *spec-output* "~@<~%#?~S ~_:be-the ~S~:>" form
          (if (y-or-n-p "~S~%Is it expected return type?" (type-of result))
              (type-of result)
              (restart-case (error 'unexpected-behavior)
                (use-value (expected)
                    :report "Specify expected type."
                    :interactive (lambda ()
                                   (list
                                     (prompt-for:prompt-for t
                                                            "Input expected type. >> ")))
                  expected))))
  (force-output *spec-output*))

(defmethod spec-of ((d (eql :default)) form args)
  (destructuring-bind
      (condition output result)
      args
    (funcall (formatter "~%~<#?~S ~_=> ~S~@[~%~A~]~@[~%~A~]~@[~%~A~]~:>")
             *spec-output*
             (list form
                   (if (y-or-n-p "~S~%Expected return value?" result)
                       result
                       (restart-case (error 'unexpected-behavior)
                         (use-value (expected)
                             :report "Specify expected value."
                             :interactive (lambda ()
                                            (list
                                              (prompt-for:prompt-for t
                                                                     "Input expected result. >> ")))
                           expected)))
                   (unless (typep result '(or symbol character integer))
                     ", :test equal")
                   (when (typep condition 'warning)
                     ", :ignore-signals warning")
                   (unless (equal "" output)
                     ", :stream nil"))))
  (force-output *spec-output*))

(defun unreadable-objectp (object)
  (uiop:string-prefix-p "#<" (prin1-to-string object)))

(defun read-expected ()
  (format *query-io* "Type expected output till Ctl-D.~%")
  (force-output *query-io*)
  (loop :for line := (read-line *query-io* nil nil)
        :while line
        :collect line :into lines
        :finally (return (format nil "~{~A~^~%~}" lines))))

(defun dribble-print (&rest values)
  (map nil #'print values)
  (force-output)
  (values))