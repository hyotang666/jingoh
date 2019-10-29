(in-package :jingoh.generator)

(defmethod generate((dispatcher (eql :readme))&key system)
  (declare(ignore dispatcher))
  (setf system (asdf:find-system system)) ; as canonicalize.
  (let*((readme-path
	  (Path-of "README" "md"(asdf:system-source-directory system)))
	(existp
	  (probe-file readme-path))
	(lines
	  (and existp
	       (uiop:read-file-lines readme-path))))
    (Output-to readme-path
	       (if (or existp
		       lines)
		 (lambda()
		   (readme-updator system lines))
		 (readme-generator system)))))

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
