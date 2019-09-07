(in-package :jingoh.generator)

(defmethod generate((dispatcher (eql :readme))&key system)
  (declare(ignore dispatcher))
  (setf system (asdf:find-system system)) ; as canonicalize.
  (let*((ssd
	  (asdf:system-source-directory system))
	(readme-path
	  (uiop:subpathname ssd "README.md"))
	(existp
	  (probe-file readme-path))
	(lines
	  (and existp
	       (uiop:read-file-lines readme-path))))
    (uiop:with-output-file(*standard-output* readme-path :if-exists :supersede
					     :if-does-not-exist :create)
      (if (or existp
	      lines)
	(readme-updator system lines)
	(funcall(readme-generator system))))))

(defun readme-updator(system readme-lines)
  (format t "# ~@:(~A~) ~A~%"(asdf:coerce-name system)
	  (or (asdf:component-version system)
	      "0.0.0"))
  (labels((REC(lines)
	    (unless(endp lines)
	      (BODY(car lines)(cdr lines))))
	  (BODY(line rest)
	    (cond
	      ((uiop:string-prefix-p "## What is this?" line)
	       (write-line line)
	       (write-line (or (asdf:system-description system)
			       ""))
	       (REC(SKIP-TO "##" rest)))
	      ((uiop:string-prefix-p "### License"line)
	       (write-line line)
	       (write-line (or (asdf:system-license system)
			       "TODO"))
	       (REC(SKIP-TO "##" rest)))
	      (t (write-line line)
		 (REC rest))))
	  (SKIP-TO(prefix rest)
	    (nthcdr (position-if (lambda(line)
				   (uiop:string-prefix-p prefix line))
				 rest)
		    rest))
	  )
    (REC (cdr readme-lines))))
