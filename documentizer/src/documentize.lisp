(in-package :jingoh.documentizer)

;;;; UTILITIES
(defun index-chars(symbols)
  (sort (delete-duplicates(mapcar #'first-char symbols))
	#'char<))

(defun first-char(symbol)
  (char-upcase(char(symbol-name symbol)0)))

;;;; VARIABLE for debug use.
(defvar *meta* nil)

;;;; DOCUMENTIZE
(defun documentize(system)
  (let*((system(asdf:find-system system))
	(sys-dir(asdf:system-source-directory system)))
    (let((meta-datas(Meta-datas<=system system sys-dir)))
      (when meta-datas
	(setf *meta* meta-datas) ; for debug use.
	(let((*default-pathname-defaults*(merge-pathnames "docs/" sys-dir)))
	  (ensure-directories-exist *default-pathname-defaults*)
	  (top system)
	  (packages meta-datas)
	  (symbol-index meta-datas system)
	  (dolist(m meta-datas)
	    (about-package m)
	    (about-symbols m))
	  (table meta-datas #'table-callback)
	  *default-pathname-defaults*)))))

;;; TOP
(defun top(system)
  (With-doc-directory((merge-pathnames "top.html"))
    (%top system)))

(defun %top(system)
  (format t "# ~A~%~@[## ~A~%~]~@[~A~2%~]~{~D. [~A](~A)~&~}"
	  (asdf:coerce-name system)
	  (asdf:system-description system)
	  (asdf:system-long-description system)
	  (loop :for i :upfrom 1
		:for title :in '(packages symbols)
		:collect i
		:collect (symbol-name title)
		:collect (Target-path (string-downcase(symbol-name title))))))

;;; PACKAGES
(defun packages(meta-datas)
  (With-doc-directory((merge-pathnames "packages.html"))
    (%packages meta-datas)))

(defun %packages(meta-datas)
  (format t "# Packages Index~%")
  (loop :for i :upfrom 1
	:for m :in meta-datas
	:do (format t "~D. [~A](~A)~%"
		    i
		    (string(Meta-data-name m))
		    (Target-path (format nil "P_~A"(Meta-data-name m))))))

;;; SYMBOL-INDEX
(defun symbol-index(meta-datas system)
  (With-doc-directory((merge-pathnames(Target-path "symbols")))
    (%symbol-index meta-datas system)))

(defun %symbol-index (meta-datas system)
  (labels((LINKS(chars &optional(code #.(char-code #\A)) have-non-alph-p acc)
	    (if(not(<= code #.(char-code #\Z)))
	      (nreconc acc (or have-non-alph-p '("Non-Alphabetic")))
	      (let((char(car chars)))
		(if(null char)
		  (LINKS chars (1+ code) have-non-alph-p
			 (push (princ-to-string (code-char code))acc))
		  (if(not(alpha-char-p char))
		    (LINKS (cdr chars) code
			   (pushnew (format nil "[Non-Alphabetic](~A)" *x-non-alph-namestring*)
				    have-non-alph-p :test #'string=)
			   acc)
		    (if(= code (char-code char))
		      (LINKS (cdr chars) (1+ code) have-non-alph-p
			     (push (format nil "[~A](~A)"
					   char
					   (X-alph-pathname char))
				   acc))
		      (LINKS chars (1+ code) have-non-alph-p
			     (push (princ-to-string (code-char code))acc)))))))))
    (let*((symbols(apply #'append (mapcar #'Meta-data-specifieds meta-datas)))
	  (index-chars(index-chars symbols)))
      (format t "# Alphabetical Symbol Index~2%There ~:[is~;are~] ~D symbol~:*~P by ~A.~2%~{~A~^ | ~}"
	      (not(= 1(length symbols)))
	      (length symbols)
	      (asdf:coerce-name system)
	      (LINKS index-chars)))))

;;; ABOUT-PACKAGE
(defun about-package(meta-data)
  (With-doc-directory((merge-pathnames(format nil "P_~A.html"(Meta-data-name meta-data))
))
    (%about-package meta-data)))

(defun %about-package (meta-data)
  (format t "# ~A~%~@[## ~:*~A Concepts~%~A~%~]## ~A Dictionary~2%~{* ~A~&~}"
	  (Meta-data-name meta-data)
	  (Meta-data-doc meta-data)
	  (Meta-data-name meta-data)
	  (labels((REC(exports &optional acc)
		    (if(endp exports)
		      (nreverse acc)
		      (BODY (car exports)(cdr exports)acc)))
		  (FIND-IN-SEC(symbol sections)
		    (find-if (lambda(section)
			       (find symbol (Section-names section)
				     :test #'string=))
			     sections))
		  (MARKUP(symbol sec)
		    (format nil "[~A](~A)"
			    (Escape-* symbol)
			    (Section-path sec)))
		  (BODY(symbol rest acc)
		    (let((sec(FIND-IN-SEC symbol
					  (Meta-data-singles meta-data))))
		      (if sec
			(REC rest (push (MARKUP symbol sec) acc))
			(let((sec(FIND-IN-SEC symbol
					      (Meta-data-commons meta-data))))
			  (if sec
			    (REC rest (push (MARKUP symbol sec) acc))
			    (REC rest (push (Escape-* symbol) acc))))))))
	    (REC (Meta-data-exports meta-data)))))

;;; ABOUT-SYMBOLS
(defun about-symbols(meta-data)
  (flet((PUT(section)
	  (With-doc-directory((merge-pathnames(Section-path section)))
	    (princ section))))
    (dolist(section(Meta-data-singles meta-data))
      (PUT section))
    (dolist(section(Meta-data-commons meta-data))
      (PUT section))))

;;; TABLE
(defun table(meta-datas &optional(callback #'table-printer))
  (labels((LINE(sec)
	    (loop :for name :in (Section-names sec)
		  :collect (cons name
				 (format nil "[~A](~A)~%"
					 (Escape-* name)
					 (Section-path sec))))))
    (let*((pairs(loop :for sec :in (apply #'append (mapcar #'Meta-data-sections meta-datas))
		      :nconc (LINE sec) :into result
		      :finally (return (sort result #'string< :key #'car))))
	  (index-chars(index-chars(apply #'append (mapcar #'Meta-data-specifieds meta-datas)))))
      (labels((REC(chars pairs)
		(unless(endp chars)
		  (apply #'REC (funcall callback chars pairs)))))
	(REC index-chars pairs)))))

(defun table-callback(chars pairs)
  (let((char (car chars))
       return)
    (With-doc-directory((if(alpha-char-p char)
			  (X-alph-pathname char)
			  *X-non-alph-namestring*))
      (setf return(table-printer chars pairs)))
    return))

(defun table-printer(chars pairs)
  (if(endp pairs)
    (list nil nil)
    (if(char= (car chars) (first-char(caar pairs)))
      (progn #1=(format t "* ~A~&"(cdar pairs))
	     (table-printer chars (cdr pairs))) ; cdring pairs.
      (if(alpha-char-p(car chars))
	#0=(list (cdr chars)pairs) ; cdring chars.
	(if(alpha-char-p(first-char(caar pairs)))
	  #0# ; cdring chars.
	  (progn #1#
		 (table-printer (cdr chars)(cdr pairs)))))))) ; cdring pairs and chars.
