(in-package :jingoh.documentizer)

(defvar *meta* nil)
(defun documentize(system)
  (let*((system(asdf:find-system system))
	(sys-dir(asdf:system-source-directory system)))
    (let((meta-datas(meta-datas<=system system sys-dir)))
      (when meta-datas
	(setf *meta* meta-datas) ; for debug use.
	(let((*default-pathname-defaults*(merge-pathnames "docs/" sys-dir)))
	  (ensure-directories-exist *default-pathname-defaults*)
	  (Top system)
	  (Packages meta-datas)
	  (Symbol-index meta-datas system)
	  (dolist(m meta-datas)
	    (About-package m)
	    (About-symbols m))
	  (Table meta-datas #'Table-callback)
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
	  (index-chars(Index-chars symbols)))
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
	  (index-chars(Index-chars(apply #'append (mapcar #'Meta-data-specifieds meta-datas)))))
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
    (if(char= (car chars) (First-char(caar pairs)))
      (progn #1=(format t "* ~A~&"(cdar pairs))
	     (table-printer chars (cdr pairs))) ; cdring pairs.
      (if(alpha-char-p(car chars))
	#0=(list (cdr chars)pairs) ; cdring chars.
	(if(alpha-char-p(First-char(caar pairs)))
	  #0# ; cdring chars.
	  (progn #1#
		 (table-printer (cdr chars)(cdr pairs)))))))) ; cdring pairs and chars.

(defun meta-datas<=system(system
			   &optional
			   (sys-dir(asdf:system-source-directory system)))
  (let((spec-dir(merge-pathnames "spec/" sys-dir)))
    (when(not(uiop:directory-exists-p spec-dir))
      (return-from meta-datas<=system (warn "Spec file is not found.")))
    (mapc #'asdf:load-system (asdf:system-depends-on system))
    (let(meta-datas)
      (let((*macroexpand-hook*
	     (let((outer-hook *macroexpand-hook*))
	       (lambda(expander form env)
		 (when(typep form '(cons (eql defpackage)T))
		   (push form meta-datas))
		 (funcall outer-hook expander form env))))
	   (asdf::*asdf-session* nil))
	(asdf:load-system system :force t))
      (let((*default-pathname-defaults* spec-dir))
	(mapcar #'Make-meta-data meta-datas)))))

(defun importer(form)
  (when(probe-file(make-pathname :name (string-downcase(string(second form)))
				 :type "lisp"
				 :defaults *default-pathname-defaults*))
    (let((meta-data
	   (Make-meta-data form)))
      (loop :for s :in (meta-data-sections meta-data)
	    :append
	    (loop :for name :in (Section-names s)
		  :when (Section-doc-type s)
		  :collect
		  `(defmethod documentation
		     ((s (eql ',(find-symbol (symbol-name name)
					     (meta-data-name meta-data))))
		      (type (eql ',(Section-doc-type s))))
		     (declare(ignore s))
		     ,(princ-to-string s)))))))

(defun lisp(system)
  (let*((system
	  (asdf:find-system system))
	(sys-dir
	  (asdf:system-source-directory system))
	(meta-datas
	  (meta-datas<=system system sys-dir))
	(*default-pathname-defaults*
	  sys-dir)
	(*package*
	  #.(find-package :jingoh.documentizer)))
    (With-output-to((merge-pathnames "doc.lisp"))
      (dolist(meta meta-datas)
	(print `(in-package ,(meta-data-name meta)))
	(dolist(s(meta-data-sections meta))
	  (print-doc s (meta-data-name meta)))))))

(defun print-doc(section package)
  (dolist(s (Section-names section))
    (when (Section-doc-type section)
      (print `(defmethod documentation ((s (eql (or (find-symbol ,(string s)
								 ,package)
						    (error "Not found ~S in ~S"
							   ,(string s)
							   ,package))))
					(type (eql ',(Section-doc-type section))))
		(declare(ignore s type))
		,(princ-to-string section))))))

(defun import(system)
  (dolist(m (meta-datas<=system (asdf:find-system system)))
    (dolist(s (meta-data-sections m))
      (dolist(name (Section-names s))
	(setf (documentation (find-symbol (symbol-name name)
					  (meta-data-name m))
			     (Section-doc-type s))
	      (princ-to-string s))))))
