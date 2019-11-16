(in-package :jingoh.documentizer)

;;;; UTILITIES
(defmacro with-open-markdown((name)&body body)
  `(WITH-OUTPUT-TO((MAKE-PATHNAME :NAME ,name
				  :TYPE "md"
				  :DEFAULTS *DEFAULT-PATHNAME-DEFAULTS*))
     ,@body))

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
      (with-open-markdown("home")
	(Top system))
      (packages.md meta-datas)
      (symbol-index.md meta-datas system)
      (dolist(m meta-datas)
	(about-package.md m)
	(About-symbols m #'section-callback.md))
      (Table meta-datas #'table-callback.md)
      *default-pathname-defaults*)))

(defun symbol-index.md(meta-datas system)
  (with-open-markdown("symbols")
    (Symbol-index meta-datas system)))

(defun packages.md(meta-datas)
  (with-open-markdown("packages")
    (Packages meta-datas)))

(defun about-package.md(meta-data)
  (with-open-markdown((format nil "P_~A"(Meta-data-name meta-data)))
    (About-package meta-data)))

(defun section-callback.md(section)
  (with-open-markdown((namestring(Section-path section)))
    (princ section)))

(defun table-callback.md(chars pairs)
  (let((char (car chars))
       return)
    (with-open-markdown((if(alpha-char-p char)
			  (namestring(X-alph-pathname char))
			  *X-non-alph-namestring*))
      (setf return(table-printer chars pairs)))
    return))

