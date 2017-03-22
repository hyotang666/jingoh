(in-package :jingoh.documentizer)

(defun replace-invalid-chars(arg)
  (let((new(string-downcase(string arg))))
    (loop :for c :across new
	  :for n :upfrom 0
	  :when (and (not(alphanumericp c))
		     (not(char= #\. c)))
	  :do (setf(char new n)(char(char-name c)0))
	  :finally(return new))))

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
  (html (format nil "X_Alph_~A"char)))

(defconstant +x-non-alph-namestring+ "X_NonAlpha.html")

(defun html(name)
  (make-pathname :name name :type "html"))

(defun index-chars(symbols)
  (sort (delete-duplicates(mapcar #'first-char symbols))
	#'char<))
