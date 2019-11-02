(in-package :jingoh.documentizer)

;;;; GITHUB-WIKI
(defun github-wiki(system &optional(pathname (uiop:getcwd)))
  (let*((system(asdf:find-system system))
	(*target-type* nil)
	(*x-non-alph-namestring* "X_NonAlpha")
	(*default-pathname-defaults* (pathname pathname))
	(meta-datas(Meta-datas<=system system (asdf:system-source-directory system)))
	)
    (when meta-datas
      (setf *Meta* meta-datas) ; for debug use.
      (With-open-markdown("home")
	(%Top system))
      (packages.md meta-datas)
      (symbol-index.md meta-datas system)
      (dolist(m meta-datas)
	(about-package.md m)
	(about-symbols.md m))
      (Table meta-datas #'table-callback.md)
      *default-pathname-defaults*)))

(defun symbol-index.md(meta-datas system)
  (With-open-markdown("symbols")
    (%Symbol-index meta-datas system)))

(defun packages.md(meta-datas)
  (With-open-markdown("packages")
    (%Packages meta-datas)))

(defun about-package.md(meta-data)
  (With-open-markdown((format nil "P_~A"(Meta-data-name meta-data)))
    (%About-package meta-data)))

(defun about-symbols.md(meta-data)
  (flet((PUT(section)
	  (With-open-markdown((namestring(Section-path section)))
	    (princ section))))
    (dolist(section(Meta-data-singles meta-data))
      (PUT section))
    (dolist(section(Meta-data-commons meta-data))
      (PUT section))))

(defun table-callback.md(chars pairs)
  (let((char (car chars))
       return)
    (With-open-markdown((if(alpha-char-p char)
			  (namestring(X-alph-pathname char))
			  *X-non-alph-namestring*))
      (setf return(table-printer chars pairs)))
    return))

