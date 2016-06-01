(in-package :cl-user)

(defpackage :jingoh.util(:use :cl)
  (:export
    #:doc
    ))
(in-package :jingoh.util)

;;;; doc
(defun doc(system &optional(subpathname #P""))
  (uiop:read-file-string
    (uiop:subpathname (asdf:system-source-directory(asdf:find-system system))
		      subpathname)))
