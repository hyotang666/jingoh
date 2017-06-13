(in-package :jingoh.documentizer)

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

