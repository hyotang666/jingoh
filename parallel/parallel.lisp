(defpackage #:jingoh.parallel
  (:use #:common-lisp #:resignal-bind #:jingoh.org #:jingoh.examiner)
  (:import-from :lparallel #:pmap #:psome #:Pmapcan #:Premove
		#:make-kernel #:*kernel*)
  (:import-from :jingoh.tester #:check #:*print-vivid*)
  (:import-from :jingoh.org #:Spec-requirements #:Spec-subject)
  (:import-from :jingoh.examiner #:print-dot #:Print-summary #:Break-on-finish)
  (:import-from :with-fields #:For-each-line)
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

(defun print-progress(subject)
  (Pmapcan (lambda(spec)
	     (let*((result(Pmap 'list #'Check (Spec-requirements spec)))
		   (subject(Spec-subject spec))
		   (lock(bt:make-lock(symbol-name subject))))
	       (when(<= 2 *verbose*)
		 (bt:with-lock-held(lock)
		   (format t "~&~S"subject)
		   (mapc #'Print-dot result)))
	       (Premove nil result)))
	   (appropriate-specs subject)))

(defun xxx-on-fails(subject)
  (Psome (lambda(spec)
	   (Psome #'Check (Spec-requirements spec)))
	 (appropriate-specs subject)))

(defun cpu-cores(&optional (default 2))
  (let((pathname(probe-file "/proc/cpuinfo")))
    (if pathname
      (For-each-line((tag 0))pathname
	(declare(:separator #\:))
	:when (uiop:string-prefix-p "processor" tag)
	:count :it)
      default)))