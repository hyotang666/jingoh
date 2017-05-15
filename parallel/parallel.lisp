(defpackage #:jingoh.parallel
  (:use #:common-lisp #:resignal-bind #:jingoh.org #:jingoh.examiner)
  (:import-from :lparallel #:pmap #:psome #:make-kernel #:*kernel*)
  (:import-from :jingoh.tester #:check)
  (:import-from :jingoh.org #:Spec-requirements #:Spec-subject)
  (:import-from :jingoh.examiner #:print-dot #:Print-summary #:Break-on-finish)
  (:export
    ;;;; main api
    #:pexamine
    ;;;; miscellaneous helper
    #:cpu-cores
    ))
(in-package #:jingoh.parallel)

(defun pexamine(org &key subject ((:verbose *Verbose*)*Verbose*)
		    ((:vivid *Print-vivid*)*Print-vivid*)
		    (cores (cpu-cores)))
  (prog*((*Org*(Resignal-bind((Missing-org()'Missing-org :api 'examine))
		 (Find-org org)))
	 (*package*(Org-package *org*))
	 (*print-circle* T)
	 (*Kernel*(Make-kernel cores)))
    (setf *Issues* (Resignal-bind((Missing-subject()
				    'Missing-subject :api 'examine))
		     (if(or *Break-on-fails* *Stop-on-fails*)
		       (xxx-on-fails subject)
		       (print-progress subject))))
    (Print-summary *Issues*)
    (when(or (<= 1 *Verbose*)
	     *Stop-on-fails*)
      (mapc #'print *Issues*)))
  (terpri)
  (when *Break-on-finish*
    (Break-on-finish *Issues*))
  )

(defun print-progress(subject &optional ignored)
  (declare(ignore ignored))
  (let((result(parallel-check subject))
       (current '#:dummy))
    (when(<= 2 *verbose*)
      (map nil (lambda(cons)
		 (unless(eq (car cons)current)
		   (setf current (car cons))
		   (format t "~&~S"current))
		 (mapc #'print-dot(cdr cons)))
	   result))
    (loop :for (nil . issues) :in result
	  :nconc (apply #'nconc issues))))

(macrolet((?!(form)
	    `(OR ,form
		 (ERROR'MISSING-SUBJECT :API 'PARALLEL-CHECK
					:DATUM subject))))
  (defun appropriate-specs(subject)
    (let((specs(Org-specifications *Org*)))
      (case subject
	((nil) specs) ; all
	((T)(loop :for sub :in (Org-current-subjects *Org*) ; current
		  :collect (find sub specs :key #'Spec-subject)))
	(otherwise (list(?!(find subject specs :key #'Spec-subject))))))))

(defun parallel-check(subject)
  (pmap 'list
	(lambda(spec)
	  (cons (spec-subject spec)
		(pmap 'list
		      (lambda(requirement)
			(check requirement))
		      (spec-requirements spec))))
	(appropriate-specs subject)))

(defun xxx-on-fails(subject)
  (psome (lambda(spec)
	   (psome #'Check (Spec-requirements spec)))
	 (appropriate-specs subject)))

(defun cpu-cores(&optional (default 2))
  (let((lscpu(uiop:run-program "which lscpu" :output :string)))
    (if(string= "" lscpu)
      default
      (let((info (uiop:run-program "lscpu | grep '^CPU(s)'" :output :string)))
	(parse-integer info :start (1+(position #\: info :test #'char=)))))))
