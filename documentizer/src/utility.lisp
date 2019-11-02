(in-package :jingoh.documentizer)

(defun escape-*(arg)
  (flet((ensure-symbol-notation(arg)
	  (if(uiop:string-suffix-p(prin1-to-string arg) "|")
	    (format nil "|~A|"arg)
	    (string arg))))
    (loop :for c :across (ensure-symbol-notation arg)
	  :when (char= #\* c)
	  :collect #\\ :into result
	  :and :collect c :into result
	  :else :collect c :into result
	  :finally (return (coerce result 'string)))))

(defun first-char(symbol)
  (char-upcase(char(symbol-name symbol)0)))

(defun x-alph-pathname(char)
  (target-path (format nil "X_Alph_~A"char)))

(defvar *x-non-alph-namestring* "X_NonAlpha.html")

(defvar *target-type* "html")
(defun target-path(name)
  (make-pathname :name name :type *target-type*))

(defun index-chars(symbols)
  (sort (delete-duplicates(mapcar #'first-char symbols))
	#'char<))

(defmacro with-output-to((pathname)&body body)
  `(WITH-OPEN-FILE(*STANDARD-OUTPUT* ,pathname
				     :DIRECTION :OUTPUT
				     :IF-EXISTS :SUPERSEDE
				     :IF-DOES-NOT-EXIST :CREATE)
     ,@body))

(defmacro with-doc-directory((pathname) &body body)
  `(WITH-OUTPUT-TO(,pathname)
     (LET((3BMD-CODE-BLOCKS:*CODE-BLOCKS* T))
       (3BMD:PARSE-STRING-AND-PRINT-TO-STREAM (WITH-OUTPUT-TO-STRING(*STANDARD-OUTPUT*)
						,@body)
	*STANDARD-OUTPUT*))))

(defmacro with-open-markdown((name)&body body)
  `(WITH-OUTPUT-TO((MAKE-PATHNAME :NAME ,name
				  :TYPE "md"
				  :DEFAULTS *DEFAULT-PATHNAME-DEFAULTS*))
     ,@body))
