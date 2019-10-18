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

(defparameter *dispatch-macro-character* #\#)

(defparameter *dispatch-macro-sub-char* #\?)

(defmacro enable(&optional(char '*dispatch-macro-sub-char*))
  (let((var (gensym "CHAR"))
       (it(gensym "TEMP")))
    `(LET*((,var ,char)
	   (,it(GET-DISPATCH-MACRO-CHARACTER *DISPATCH-MACRO-CHARACTER* ,var)))
       (IF(NULL ,it) ; Noone use it.
	 #0=(REPLACE-MACRO-CHARACTER *DISPATCH-MACRO-CHARACTER* ,var)
	 (IF(EQ '|#?reader| ,it) ; it's me!
	   #0#
	   (RESTART-CASE(ERROR'MACRO-CHAR-CONFLICTION 
			  :FORMAT-CONTROL "Dispatch macro #~C is used. ~S"
			  :FORMAT-ARGUMENTS (LIST ,var ,it))
	     (REPLACE() :REPORT "Replace it." #0#)))))))

(defun replace-macro-character(char sub-char)
  (check-type char character)
  (check-type sub-char character)
  (setf *dispatch-macro-character* char
	*dispatch-macro-sub-char* sub-char)
  (set-dispatch-macro-character char sub-char '|#?reader|))

(define-condition macro-char-confliction(simple-error)())

(defparameter *read-verbose* NIL)
(defparameter *read-print* NIL)

(defparameter *lines* nil)

(defvar *line*)

(defun |#?reader|(stream character number)
  (declare(ignore character))
  (unless *lines*
    (let((pathname
	   (ignore-errors(pathname stream))))
      (when pathname
	(setf *lines* (collect-spec-lines pathname)))))
  (let((*line*
	 1))
    (|#?reader-body| stream number)))

(defun |#?reader-body| (stream number)
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
	      (#\newline
	       (|line-counter| stream (read-char stream))
	       (have-option?))
	      (#\, (read-char stream t t t))
	      (#\;
	       (|line-comment| stream (read-char stream))
	       (have-option?))))
	  )
    (let((form `(DEFSPEC ,(read-form '#:test-form)
			 ,(read-form '#:keyword)
			 ,(read-form '#:expected)
			 :LINE ,(pop *lines*)
			 ,@(options))))
      (when (or *read-verbose* *read-print*)
	(format *trace-output* "~%READ: ~S"form))
      form)))

(defreadtable syntax
  (:merge :standard)
  (:dispatch-macro-char *dispatch-macro-character* *dispatch-macro-sub-char*
			'|#?reader|))

;;;; COLLECT-SPEC-LINES
(defvar *line-pos*)

(defun collect-spec-lines(pathname)
  (let((*readtable*
	 (named-readtables:find-readtable 'counter))
       (*line-pos*)
       (*line* 1)
       )
    ;; To set dispatch macro dynamically, since it may be constomized.
    (set-dispatch-macro-character *dispatch-macro-character* *dispatch-macro-sub-char*
				  '|#?counter|)
    (with-open-file(s pathname)
      (loop :with tag = '#:end
	    :for sexp = (read s nil tag)
	    :until (eq sexp tag)))
    (nreverse *line-pos*)))

(defun |line-counter|(stream character)
  (declare(ignore stream character))
  (incf *line*)
  (values))

(defun |#?counter|(stream character number)
  (declare(ignore character))
  (push *line* *line-pos*)
  (|#?reader-body| stream number))

(defun |line-comment|(stream character)
  (declare(ignore character))
  (read-line stream)
  (incf *line*)
  (values))

(defun |block-comment|(stream character number)
  (declare(ignore character number))
  (loop :for char = (read-char stream nil nil)
	:while char
	:do
	(case char
	  (#\#
	   (let((char
		  (peek-char nil stream t t t)))
	     (case char
	       (#\| ; nested comment.
		(|block-comment| stream char nil))
	       ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
		(loop :initially (read-char stream)
		      :for char = (peek-char nil stream)
		      :while (digit-char-p char)
		      :do (read-char stream))
		(if(char= #\| (read-char stream)) ; nested comment with number, e.g. #1|.
		  (|block-comment| stream #\| nil)))
	       (#\newline
		(|line-counter| stream (read-char stream)))
	       (otherwise
		 #|Do nothing, to next loop|#))))
	  (#\newline
	   (|line-counter| stream char))
	  (#\|
	   (if(char= #\# (peek-char nil stream))
	     (progn (read-char stream)
		    (loop-finish))
	     #|Do nothing, to next loop|#))
	  (otherwise #|Do nothing, to next loop|#)))
  (values))

(defreadtable counter
  (:merge :standard)
  (:macro-char #\newline '|line-counter|)
  (:macro-char #\; '|line-comment|)
  (:dispatch-macro-char #\# #\| '|block-comment|)
  )
