(defpackage :jingoh.examiner(:use :cl :jingoh.org :jingoh.tester :resignal-bind)
  (:export
    ;;;; main api
    #:examine

    ;;;; variables
    #:*verbose*
    #:*on-fails*
    #:*break-on-finish*
    #:*issues*

    ;;;; DSL
    #:with-examiner-context
    ))
(in-package :jingoh.examiner)

; subject, detail, summary
(defparameter *verbose* 2 "Controls VERIFY's verbosity.")
(defparameter *on-fails* NIL)
(declaim(type (member :error :stop nil)*on-fails*))
(defparameter *break-on-finish* NIL "Breaks when finish examine.")
(defparameter *issues* NIL "Previous issues. Debug use.")
(defparameter *requirement-form* nil "Previous test form. Debug use.")

(defmacro with-examiner-context(form)
  `(LET((*PRINT-CIRCLE* T)
	(*PRINT-LENGTH* NIL) ; for sbcl at least.
	(*PRINT-LEVEL* NIL) ; for sbcl at least.
	(*PRINT-PRETTY* T) ; for ccl at least.
	)
     ,form))

(define-condition break-on-fails(error)())
(defun break-on-fails(result)
  (with-examiner-context(print result))
  (invoke-debugger(make-condition 'break-on-fails)))

(define-condition break-on-finish(error)
  ((issues :initarg :issues :reader issues))
  (:report(lambda(condition stream)
	      (declare(ignore condition))
	      (format stream "Break cause *BREAK-ON-FINISH*."))))
(defun break-on-finish(&optional issues)
  (invoke-debugger(make-condition 'break-on-finish :issues issues)))
(defmethod print-object((c break-on-finish)stream)
  (if *print-escape*
    (print-unreadable-object(c stream)
      (princ (string-trim `(#\newline)
			  (with-output-to-string(out)
			    (print-summary (issues c)out)))
	     stream))
    (call-next-method)))

(defun print-progress(subject &optional (goto #'identity))
  (let((current-subject '#:dummy)
       (issues))
    (Do-requirements((requirement sub)subject)
      (let((result(Check requirement)))
	(push result issues)
	(when result
	  (setf *requirement-form* (Requirement-form requirement))
	  (case *on-fails*
	    ((:error)
	     (setf *issues* (apply #'nconc (nreverse issues)))
	     (break-on-fails result))
	    ((:stop)
	     (setf *issues* (apply #'nconc (nreverse issues)))
	     (format t "~2%; Stop to examine cause *ON-FAILS* at ~A~%"sub)
	     (funcall goto))))
	(when(<= 2 *verbose*)
	  (unless(eq sub current-subject)
	    (setf current-subject sub)
	    (format t "~&~S"current-subject))
	  (print-dot result))))
    (apply #'nconc (nreverse issues))))

(defun print-dot(result)
  (if result
    (if *print-vivid*
      (cl-ansi-text:with-color(:red)
	(write-char #\!))
      (write-char #\!))
    (if *print-vivid*
      (cl-ansi-text:with-color(:green)
	(write-char #\.))
      (write-char #\.)))
  (force-output))

(defun print-summary(issues &optional (*standard-output* *standard-output*))
  (if(zerop(Org-requirements-count *org*))
    (warn "No requirements in ~S"(Org-name *org*))
    (let((count(length issues)))
      (if (zerop count)
	(format t "~&~A ~S"
		(if *print-vivid*
		  (cl-ansi-text:green "Pass")
		  "Pass")
		(Org-name *org*))
	(format t "~&~A in ~S"
		(if *print-vivid*
		  (cl-ansi-text:red #0=(format nil "~D fail~:*~P"count))
		  #0#)
		(Org-name *org*))))))

(defun examine(org &key subject ((:verbose *verbose*)*verbose*)
		   ((:vivid *print-vivid*)*print-vivid*))
  (setf *issues* NIL)
  (prog*((*org*(resignal-bind((missing-org()'missing-org :api 'examine))
		 (find-org org)))
	 (*package*(Org-package *org*)))
    ;; in order to be able to see tag, we need SETF in PROG*'s body.
    (setf *issues* (resignal-bind((missing-subject()
				    'missing-subject :api 'examine))
		     (print-progress subject(lambda()(go #0=#:end)))))
    (print-summary *issues*)
    #0#
    (when(or (<= 1 *verbose*)
	     (eq :stop *on-fails*))
      (with-examiner-context
	(mapc #'print *issues*))))
  (terpri)
  (when *break-on-finish*
    (break-on-finish *issues*))
  )

