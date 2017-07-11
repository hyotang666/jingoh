(defpackage :jingoh.documentizer.utility(:use :cl)
  (:export
    #:replace-invalid-chars
    #:escape-*
    #:first-char
    #:x-alph-pathname
    #:*x-non-alph-namestring*
    #:*target-type*
    #:target-path
    #:index-chars
    ))
(in-package :jingoh.documentizer.utility)

(defun replace-invalid-chars(arg)
  (loop :for c :across (string-downcase(string arg))
	:for n :upfrom 0
	:when (and (not(alphanumericp c))
		   (not(char= #\. c)))
	:collect (princ-to-string(char-code c)) :into result
	:else :collect c :into result
	:finally(return (uiop:reduce/strcat result))))

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
