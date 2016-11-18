(in-package :cl-user)
(defpackage :jingoh.reader(:use :cl :jingoh.tester :named-readtables)
  (:import-from :documentation-embedder #:Doc)
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
  #.(Doc :jingoh.reader "doc/enable.M.md")
  (let((var (gensym "CHAR"))
       (it(gensym "TEMP")))
    `(EVAL-WHEN(:LOAD-TOPLEVEL :COMPILE-TOPLEVEL :EXECUTE)
       (LET*((,var ,char)
	     (,it(GET-DISPATCH-MACRO-CHARACTER #\# ,var)))
	 (IF(NULL ,it) ; Noone use it.
	   #0=(REPLACE-MACRO-CHARACTER ,var)
	   (IF(EQ '|#?reader| (MILLET:FUNCTION-NAME ,it)) ; it's me!
	     #0#
	     (RESTART-CASE(ERROR'MACRO-CHAR-CONFLICTION 
			    :FORMAT-CONTROL "Dispatch macro #~C is used. ~S"
			    :FORMAT-ARGUMENTS (LIST ,var ,it))
	       (REPLACE() :REPORT "Replace it." #0#))))))))

(defun replace-macro-character(char)
  #.(Doc :jingoh.reader "doc/replace-macro-character.F.md")
  (set-dispatch-macro-character #\# char #'|#?reader|))

(define-condition macro-char-confliction(simple-error)()
  (:documentation #.(Doc :jingoh.reader "doc/macro-char-confliction.T.md")))

(defparameter *read-verbose* NIL)
(defparameter *read-print* NIL)

(defun |#?reader|(stream character number)
  #.(Doc :jingoh.reader "doc/#Qreader.F.md")
  (declare(ignore character))
  (let((position(file-position stream)))
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
      (let((form `(EVAL-WHEN(:COMPILE-TOPLEVEL :LOAD-TOPLEVEL)
		    (DEFSPEC ,(read-form '#:test-form)
			     ,(read-form '#:keyword)
			     ,(read-form '#:expected)
			     :POSITION ,position
			     ,@(options)))))
	(when (or *read-verbose* *read-print*)
	  (format *trace-output* "~%READ: ~S"form))
	form))))

(defreadtable syntax
  (:merge :standard)
  (:dispatch-macro-char #\# #\? #'|#?reader|)
  (:dispatch-macro-char #\# #\` #'musam:|#`reader|))

