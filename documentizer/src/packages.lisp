(in-package :jingoh.documentizer)

(defun packages(meta-datas)
  (with-doc-directory((merge-pathnames "packages.html"))
    (%packages meta-datas)))

(defun %packages(meta-datas)
  (format t "# Packages Index~%")
  (loop :for i :upfrom 1
	:for m :in meta-datas
	:do (format t "~D. [~A](~A)~%"
		    i
		    (string(meta-data-name m))
		    (html (format nil "P_~A"(meta-data-name m))))))
