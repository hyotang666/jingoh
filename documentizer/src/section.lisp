(defpackage :jingoh.documentizer.sections
  (:use :cl)
  (:import-from :jingoh.documentizer.utility #:Escape-*)
  (:export
    ; Constructor
    #:make-single
    #:make-common
    ; slot readers
    #:section-body
    #:section-path
    #:section-names
    #:section-doc-type
    ; subtype predicates
    #:single-p
    #:common-p
    )
  )
(in-package :jingoh.documentizer.sections)

(defstruct section body path names doc-type)
(defstruct(single (:include section)
		  (:constructor make-single (&key body path name doc-type
						  &aux(names (list name))))))
(defstruct(common (:include section))
  alias)

(defmethod print-object ((obj single)*standard-output*)
  (if *print-escape*
    (print-unreadable-object(obj *standard-output* :type nil :identity nil)
      (prin1 (car(section-names obj))))
    (call-next-method)))

(defmethod print-object((obj common)*standard-output*)
  (if *print-escape*
    (print-unreadable-object(obj *standard-output* :type nil :identity nil)
      (prin1 (common-alias obj)))
    (call-next-method)))

(defmethod print-object((obj section)*standard-output*)
  (if *print-escape*
    (call-next-method)
    (progn 
      (format t "# ~{~A~^, ~}~&"(mapcar #'Escape-*(Section-names obj)))
      (princ-section-body(section-body obj)))))

(defun princ-section-body (body)
  (labels((REC(list)
	    (unless(endp list)
	      (BODY (car list)(cdr list))))
	  (BODY(elt rest)
	    (cond
	      ((uiop:string-prefix-p ";;;; " elt)
	       (write-string "## ")
	       (write-line elt *standard-output* :start 5))
	      ((uiop:string-prefix-p ";;; " elt)
	       (write-string "### ")
	       (write-line elt *standard-output* :start 4))
	      ((uiop:string-prefix-p ";; " elt)
	       (write-string "#### ")
	       (write-line elt *standard-output* :start 3))
	      ((uiop:string-prefix-p "; " elt)
	       (write-line elt *standard-output* :start 2))
	      ((uiop:string-prefix-p "#?" elt)
	       (write-line "```lisp")
	       (write-line elt)
	       (write-string (car rest))
	       (write-char #\space)
	       (write-line (cadr rest))
	       (loop :with ops = 0
		     :for (key value) :on (cddr rest) :by #'cddr
		     :while (uiop:string-prefix-p "," key)
		     :do (write-string key)
		     (write-char #\space)
		     (write-line value)
		     (incf ops)
		     :finally (format t "```~2%")
		     (setf rest (nthcdr (+ 2 (* 2 ops))rest))))
	      ((uiop:string-prefix-p "#+syntax" elt)
	       (write-string "### ")
	       (write-line (Escape-* elt) *standard-output* :start 2))
	      ((uiop:string-prefix-p "#+setf" elt)
	       (write-string "### ")
	       (write-line (escape-* elt) *standard-output* :start 2))
	      ((uiop:string-prefix-p "#+signature" elt)
	       (write-string "### ")
	       (write-line (escape-* elt) *standard-output* :start 12))
	      ((uiop:string-prefix-p "#| " elt)
	       (write-string "# ")
	       (write-line elt *standard-output* 
			   :start 3 :end (- (length elt) 2)))
	      (t nil)); discard
	    (REC rest)))
    (REC body)))
