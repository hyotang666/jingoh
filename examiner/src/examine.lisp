(defpackage :jingoh.examiner(:use :cl :jingoh.org :jingoh.tester :resignal-bind)
  (:export
    ;;;; main api
    #:examine

    ;;;; variables
    #:*verbose*
    #:*stop-on-fails*
    #:*break-on-fails*
    #:*break-on-finish*
    #:*issues*
    ))
(in-package :jingoh.examiner)

; subject, detail, summary
(defparameter *verbose* 2 "Controls VERIFY's verbosity.")
(defparameter *stop-on-fails* NIL "Stop rest verifying when fails.")
(defparameter *break-on-fails* NIL "Breaks when fails")
(defparameter *break-on-finish* NIL "Breaks when finish examine.")
(defparameter *issues* NIL "Previous issues. Debug use.")
(defparameter *requirement-form* nil "Previous test form. Debug use.")

(define-condition break-on-fails(simple-condition)())
(defun break-on-fails(result)
  (invoke-debugger(make-condition 'break-on-fails
				  :format-control"~&~{~S~&~}"
				  :format-arguments `(,result))))

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
	  (setf *issues* (apply #'nconc (nreverse issues))
		*requirement-form* (Requirement-form requirement))
	  (if *break-on-fails*
	    (break-on-fails result)
	    (when *stop-on-fails*
	      (format t "~2%; Stop to examine cause *STOP-ON-FAILS* at ~A~%"sub)
	      (funcall goto))))
	(when(<= 2 *verbose*)
	  (unless(eq sub current-subject)
	    (setf current-subject sub)
	    (format t "~&~S"current-subject))
	  (if result
	    (if *print-vivid*
	      (cl-ansi-text:with-color(:red)
		(write-char #\!))
	      (write-char #\!))
	    (if *print-vivid*
	      (cl-ansi-text:with-color(:green)
		(write-char #\.))
	      (write-char #\.)))
	  (force-output))))
    (apply #'nconc (nreverse issues))))

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
		  (cl-ansi-text:red #0=(format nil "Fail ~D test~:*~P"count))
		  #0#)
		(Org-name *org*))))))

(defun examine(org &key subject ((:verbose *verbose*)*verbose*)
		   ((:vivid *print-vivid*)*print-vivid*))
  (setf *issues* NIL)
  (prog*((*org*(resignal-bind((missing-org()'missing-org :api 'examine))
		 (find-org org)))
	 (*package*(Org-package *org*))
	 (*print-circle* T))
    ;; in order to be able to see tag, we need SETF in PROG*'s body.
    (setf *issues* (resignal-bind((missing-subject()
				    'missing-subject :api 'examine))
		     (print-progress subject(lambda()(go :end)))))
    (print-summary *issues*)
    :end
    (when(or (<= 1 *verbose*)
	     *stop-on-fails*)
      (mapc #'print *issues*)))
  (terpri)
  (when *break-on-finish*
    (break-on-finish *issues*))
  )

