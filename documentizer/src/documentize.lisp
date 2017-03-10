(in-package :jingoh.documentizer)

(defvar *meta* nil)
(defun documentize(system)
  (let(meta-datas)
    (flet((HOOK(expander form env)
	    (when(typep form '(cons (eql defpackage)T))
	      (push form meta-datas))
	    (funcall expander form env)))
      (let*((system(asdf:find-system system))
	    (sys-dir(asdf:system-source-directory system))
	    (spec-dir(merge-pathnames "spec/" sys-dir)))
	(when(not(uiop:directory-exists-p spec-dir))
	  (return-from documentize (warn "Spec file is not found.")))
	(mapc #'asdf:load-system (asdf:system-depends-on system))
	(let((*default-pathname-defaults* spec-dir)
	     (*macroexpand-hook* #'HOOK))
	  (asdf:load-system system :force t)
	  (map-into meta-datas #'make-meta-data meta-datas))
	(setf *meta* meta-datas) ; for debug use.
	(let((*default-pathname-defaults*(merge-pathnames "docs/" sys-dir)))
	  (ensure-directories-exist *default-pathname-defaults*)
	  (Top system)
	  (Packages meta-datas)
	  (Symbol-index meta-datas system)
	  (dolist(m meta-datas)
	    (About-package m)
	    (About-symbols m))
	  (Table meta-datas)
	  *default-pathname-defaults*)))))

