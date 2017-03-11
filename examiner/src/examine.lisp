(in-package :jingoh.examiner)

; subject, detail, summary
(defparameter *verbose* 2 "Controls VERIFY's verbosity.")
(defparameter *stop-on-fails* NIL "Stop rest verifying when fails.")
(defparameter *break-on-fails* NIL "Breaks when fails")
(defparameter *issues* NIL "Previous issues. Debug use.")

(defun print-progress(subject &optional (goto #'identity))
  (let((current-subject '#:dummy))
    (Do-requirements((requirement sub)subject)
      (let((result(Check requirement)))
	(setf *issues* (nconc *issues* result))
	(when result
	  (if *break-on-fails*
	    (error "~&; @~S ~{~S~&~}"subject result)
	    (when *stop-on-fails*
	      (format t "~&Stop to examine cause *STOP-ON-FAILS*~&@~A"sub)
	      (funcall goto))))
	(when(<= 2 *verbose*)
	  (unless(eq sub current-subject)
	    (setf current-subject sub)
	    (format t "~&~A"current-subject))
	  (if result
	    (cl-ansi-text:with-color(:red)
	      (write-char #\!))
	    (cl-ansi-text:with-color(:green)
	      (write-char #\.)))
	  (force-output))))
    *issues*))

(defun print-summary(issues)
  (if(zerop(Org-requirements-count *org*))
    (warn "No requirements in ~S"(Org-name *org*))
    (let((count(length issues)))
      (if (zerop count)
	(format t "~&~A ~S"
		(cl-ansi-text:green "Pass")
		(Org-name *org*))
	(format t "~&~A in ~S"
		(cl-ansi-text:red (format nil "Fail ~D test~:*~P"count))
		(Org-name *org*))))))

(defun examine(&key (org *org*)subject ((:verbose *verbose*)*verbose*))
  (setf *issues* NIL)
  (prog*((*org*(find-org org))
	 (*package*(Org-package *org*)))
    ;; in order to be able to see tag, we need SETF in PROG*'s body.
    (setf *issues* (print-progress subject(lambda()(go :end))))
    (print-summary *issues*)
    :end
    (when(or (<= 1 *verbose*)
	     *stop-on-fails*)
      (mapc #'print *issues*))))

(defstruct(diff(:constructor markup (object)))
  object)
(defstruct(string-diff(:include diff)
	    (:constructor markup-string (object origin)))
  origin)
(defmethod print-object((object diff)*standard-output*)
  (cl-ansi-text:with-color(:red)
    (prin1(diff-object object))))

(defvar *color-hook* #'cl-ansi-text:red "For test, used as mock")

(defmethod print-object((object string-diff)*standard-output*)
  (let*((pre-start 0)
	(pre-end(or (mismatch (string-diff-origin object)
			      (string-diff-object object))
		    0))
	(pre-string (subseq (string-diff-object object)pre-start pre-end))
	(rev-origin(reverse(string-diff-origin object)))
	(rev-object(reverse(string-diff-object object)))
	(rev-end(mismatch rev-origin rev-object))
	(post-start(- (length(string-diff-object object))
		      rev-end))
	(post-string(nreverse(subseq rev-object 0 rev-end))))
    (prin1 (concatenate 'string
			pre-string
			(funcall *color-hook* (subseq (string-diff-object object)
						      pre-end post-start))
			(cond
			  ;; too much short
			  ((= pre-end (length (string-diff-object object)))
			   (funcall *color-hook* ":NULL"))
			  ;; too much long.
			  ((= pre-end (length (string-diff-origin object)))
			   (funcall *color-hook* post-string))
			  (t post-string))))))

(defun mismatch-sexp(sexp1 sexp2)
  (let(env)
    (labels((rec(sexp1 sexp2)
	      (typecase sexp1
		(cons (if (null sexp2)
			(markup :null)
			(if (atom sexp2)
			  (markup sexp2)
			  (cons (rec (car sexp1)(car sexp2))
				(rec (cdr sexp1)(cdr sexp2))))))
		((or boolean keyword)
		 (if(eq sexp1 sexp2)
		   sexp2
		   (markup sexp2)))
		(symbol (if(not (symbolp sexp2))
			  (markup sexp2)
			  (if(symbol-package sexp1)
			    (if(eq sexp1 sexp2)
			      sexp2
			      (markup sexp2))
			    ;; sexp1 is uninterned symbol.
			    (let((pair(assoc sexp1 env :test #'eq)))
			      (if pair ; seen.
				(if(eq sexp2 (cdr pair))
				  sexp2
				  (markup sexp2))
				; unseen.
				(let((pair(rassoc sexp2 env :test #'eq)))
				  (if pair ; seen.
				    (markup sexp2)
				    (progn (push (cons sexp1 sexp2) env)
					   sexp2))))))))
		(string (if (not(stringp sexp2))
			  (markup sexp2)
			  (if(string= sexp1 sexp2)
			    sexp2
			    (markup-string sexp2 sexp1))))
		(pathname (if (not(pathnamep sexp2))
			    (markup sexp2)
			    (if (equal sexp1 sexp2)
			      sexp2
			      (markup-string (namestring sexp2)
					     (namestring sexp1)))))
		((or number character bit-vector)
		 (if(equal sexp1 sexp2)
		   sexp2
		   (markup sexp2)))
		(vector (if (not(vectorp sexp2))
			  (markup sexp2)
			  (do*((i 0 (1+ i))
			      (a-p (array-in-bounds-p sexp1 i)(array-in-bounds-p sexp1 i))
			      (b-p (array-in-bounds-p sexp2 i)(array-in-bounds-p sexp2 i))
			      (acc))
			    ((or (and (not a-p)(not b-p))
				 (and (not a-p)b-p)
				 (and a-p(not b-p)))
			     (cond
			       ((and (not a-p)(not b-p))
				(coerce (nreverse acc)'vector))
			       ((and (not a-p)b-p)
				(concatenate 'vector (nreverse acc)
					     (map 'vector #'markup(subseq sexp2 i))))
			       ((and a-p (not b-p))
				(coerce (nreverse (cons :null acc))'vector))))
			    (push (rec (aref sexp1 i)
				       (aref sexp2 i))
				  acc))))
		(array (if (not(arrayp sexp2))
			 (markup sexp2)
			 (if(equal (array-dimensions sexp1)
				   (array-dimensions sexp2))
			   (markup (list :different-dimensions
					 (array-dimensions sexp1)
					 (array-dimensions sexp2)
					 sexp2))
			   (let((a(make-array(array-dimensions sexp2))))
			     (dotimes(i (array-total-size sexp1)a)
			       (setf (row-major-aref a i)
				     (rec (row-major-aref sexp1 i)
					  (row-major-aref sexp2 i))))))))
		(t (if (not(typep(class-of sexp1)'structure-class))
		     (if(equal sexp1 sexp2)
		       sexp2
		       (markup sexp2))
		     (if (not(eq (type-of sexp1)(type-of sexp2)))
		       (markup sexp2)
		       (loop :with object = (copy-structure sexp2)
			     :for slot1 :in (slots<=obj sexp1)
			     :for slot2 :in (slots<=obj sexp2)
			     :do (setf (slot-value object slot2)
				       (rec (slot-value sexp1 slot1)
					    (slot-value sexp2 slot2)))))))))
	    )
      (rec sexp1 sexp2))))

(defun slots<=obj(obj)
  (mapcar #'closer-mop:slot-definition-name (closer-mop:class-slots(class-of obj))))