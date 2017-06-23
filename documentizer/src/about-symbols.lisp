(in-package :jingoh.documentizer)

(defun about-symbols(meta-data)
  (flet((PUT(section)
	  (With-doc-directory((merge-pathnames(Section-path section)))
	    (princ section))))
    (dolist(section(Meta-data-singles meta-data))
      (PUT section))
    (dolist(section(Meta-data-commons meta-data))
      (PUT section))))
