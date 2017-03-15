(defpackage :jingoh.examiner(:use :cl :jingoh.org :resignal-bind :jingoh.tester)
  (:export
    ;;;; main api
    #:examine

    ;;;; variables
    #:*verbose*
    #:*stop-on-fails*
    #:*break-on-fails*
    #:*issues*
    ))
(in-package :jingoh.examiner)

; subject, detail, summary
(defparameter *verbose* 2 "Controls VERIFY's verbosity.")
(defparameter *stop-on-fails* NIL "Stop rest verifying when fails.")
(defparameter *break-on-fails* NIL "Breaks when fails")
(defparameter *issues* NIL "Previous issues. Debug use.")

(defun print-progress(subject &optional (goto #'identity))
  (let((current-subject '#:dummy))
    (Do-requirements((requirement sub)subject)
      (let((result(Check requirement)))
	(setf *issues* (nconc *issues* result))
	(when result
	  (if *break-on-fails*
	    (error "~&; @~S ~{~S~&~}"subject result)
	    (when *stop-on-fails*
	      (format t "~&Stop to examine cause *STOP-ON-FAILS*~&@~A"sub)
	      (funcall goto))))
	(when(<= 2 *verbose*)
	  (unless(eq sub current-subject)
	    (setf current-subject sub)
	    (format t "~&~A"current-subject))
	  (if result
	    (cl-ansi-text:with-color(:red)
	      (write-char #\!))
	    (cl-ansi-text:with-color(:green)
	      (write-char #\.)))
	  (force-output))))
    *issues*))

(defun print-summary(issues)
  (if(zerop(Org-requirements-count *org*))
    (warn "No requirements in ~S"(Org-name *org*))
    (let((count(length issues)))
      (if (zerop count)
	(format t "~&~A ~S"
		(cl-ansi-text:green "Pass")
		(Org-name *org*))
	(format t "~&~A in ~S"
		(cl-ansi-text:red (format nil "Fail ~D test~:*~P"count))
		(Org-name *org*))))))

(defun examine(&key (org *org*)subject ((:verbose *verbose*)*verbose*))
  (setf *issues* NIL)
  (prog*((*org*(find-org org))
	 (*package*(Org-package *org*))
	 (*print-circle* T))
    ;; in order to be able to see tag, we need SETF in PROG*'s body.
    (setf *issues* (print-progress subject(lambda()(go :end))))
    (print-summary *issues*)
    :end
    (when(or (<= 1 *verbose*)
	     *stop-on-fails*)
      (mapc #'print *issues*)))
  (fresh-line))

