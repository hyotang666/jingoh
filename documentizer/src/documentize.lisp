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

(defun meta-datas<=system(system
			   &optional
			   (sys-dir(asdf:system-source-directory system)))
  (let((spec-dir(merge-pathnames "spec/" sys-dir)))
    (when(not(uiop:directory-exists-p spec-dir))
      (return-from meta-datas<=system (warn "Spec file is not found.")))
    (mapc #'asdf:load-system (asdf:system-depends-on system))
    (let(meta-datas)
      (let((*macroexpand-hook* (lambda(expander form env)
				 (when(typep form '(cons (eql defpackage)T))
				   (push form meta-datas))
				 (funcall expander form env)))
	   (asdf::*asdf-session* nil))
	(asdf:load-system system :force t))
      (let((*default-pathname-defaults* spec-dir))
	(mapcar #'Make-meta-data meta-datas)))))

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
