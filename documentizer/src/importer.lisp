(in-package #:jingoh.documentizer)

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
