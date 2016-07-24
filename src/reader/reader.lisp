(in-package :cl-user)
(defpackage :jingoh.reader(:use :cl :jingoh.tester :named-readtables :jingoh.util)
  (:export
    ;;;; main api
    #:enable
    #:replace-macro-character
    #:|#?reader|
    
    ;;;; readtable name
    #:syntax

    ;;;; conditions
    #:macro-char-confliction
    ))
(in-package :jingoh.reader)

(defmacro enable(&optional(char #\?))
  #.(doc :jingoh.reader "doc/reader/enable.M.md")
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
  #.(doc :jingoh.reader "doc/reader/replace-macro-character.F.md")
  (set-dispatch-macro-character #\# char #'|#?reader|))

(define-condition macro-char-confliction(simple-error)()
  (:documentation #.(doc :jingoh.reader "doc/reader/macro-char-confliction.T.md")))

(defun |#?reader|(stream character number)
  #.(doc :jingoh.reader "doc/reader/#Qreader.F.md")
  (declare(ignore character number))
  (let((position(file-position stream)))
    `(EVAL-WHEN(:COMPILE-TOPLEVEL :LOAD-TOPLEVEL)
       (DEFSPEC ,(read stream t t t) ; as test form
		,(read stream t t t) ; as reserved keyword
		,(read stream t t t) ; as expected result
		:POSITION ,position
		,@(loop :for char = (peek-char t  stream nil nil)
			:while (eql #\, char)
			:do (read-char stream) ; discard #\,
			:collect (read stream t t t)
			:collect (read stream t t t))))))

(defreadtable syntax
  (:merge :standard)
  (:dispatch-macro-char #\# #\? #'|#?reader|)
  (:dispatch-macro-char #\# #\` #'musam:|#`reader|))

