(in-package :cl-user)
(defpackage :jingoh.reader(:use :cl :jingoh.tester :named-readtables)
  (:export
    ;;;; main api
    #:enable
    #:replace-macro-character
    #:|#?reader|

    ;;;; special symbol for debug/trace
    #:*read-verbose*
    #:*read-print*
    
    ;;;; readtable name
    #:syntax

    ;;;; conditions
    #:macro-char-confliction
    ))
(in-package :jingoh.reader)

(defmacro enable(&optional(char #\?))
  (let((var (gensym "CHAR"))
       (it(gensym "TEMP")))
    `(LET*((,var ,char)
	   (,it(GET-DISPATCH-MACRO-CHARACTER #\# ,var)))
       (IF(NULL ,it) ; Noone use it.
	 #0=(REPLACE-MACRO-CHARACTER ,var)
	 (IF(EQ '|#?reader| (MILLET:FUNCTION-NAME ,it)) ; it's me!
	   #0#
	   (RESTART-CASE(ERROR'MACRO-CHAR-CONFLICTION 
			  :FORMAT-CONTROL "Dispatch macro #~C is used. ~S"
			  :FORMAT-ARGUMENTS (LIST ,var ,it))
	     (REPLACE() :REPORT "Replace it." #0#)))))))

(defun replace-macro-character(char)
  (set-dispatch-macro-character #\# char #'|#?reader|))

(define-condition macro-char-confliction(simple-error)())

(defparameter *read-verbose* NIL)
(defparameter *read-print* NIL)

(defparameter *lines* nil)

(defun |#?reader|(stream character number)
  (declare(ignore character))
  (unless *lines*
    (let((pathname
	   (ignore-errors(pathname stream))))
      (when pathname
	(setf *lines* (collect-spec-lines pathname)))))
  (labels((read-form(as)
	    (let((form(read stream t t t)))
	      (when *read-print*
		(format *trace-output* "~%~S: ~S"as form))
	      form))
	  (options()
	    (if number
	      (loop :repeat number
		    :collect (read-form '#:option-key)
		    :collect (read-form '#:option-value))
	      (loop :while (have-option?)
		    :collect (read-form '#:option-key)
		    :collect (read-form '#:option-value))))
	  (have-option?()
	    (case (peek-char t stream nil nil t)
	      (#\, (read-char stream t t t))
	      (#\; (read-line stream t t t)(have-option?))))
	  )
    (let((form `(DEFSPEC ,(read-form '#:test-form)
			 ,(read-form '#:keyword)
			 ,(read-form '#:expected)
			 :LINE ,(pop *lines*)
			 ,@(options))))
      (when (or *read-verbose* *read-print*)
	(format *trace-output* "~%READ: ~S"form))
      form)))

(defun collect-spec-lines(pathname)
  (with-open-file(s pathname)
    (loop :for char = (read-char s nil)
	  :while char
	  :if (char= #\newline char)
	  :count it :into line
	  :and :if (and (eql #\# (peek-char nil s nil))
			(read-char s)
			(eql #\? (peek-char nil s nil)))
	  :collect (1+ line))))

(defreadtable syntax
  (:merge :standard)
  (:dispatch-macro-char #\# #\? #'|#?reader|))

