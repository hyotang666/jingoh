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

(defun x-alph-pathname(char)
  (target-path (format nil "X_Alph_~A"char)))

(defvar *x-non-alph-namestring* "X_NonAlpha.html")

(defvar *target-type* "html")
(defun target-path(name)
  (make-pathname :name name :type *target-type*))

(defmacro with-output-to((pathname)&body body)
  `(WITH-OPEN-FILE(*STANDARD-OUTPUT* ,pathname
				     :DIRECTION :OUTPUT
				     :IF-EXISTS :SUPERSEDE
				     :IF-DOES-NOT-EXIST :CREATE)
     ,@body))
