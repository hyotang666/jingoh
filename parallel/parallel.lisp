(defpackage #:jingoh.parallel
  (:use #:common-lisp #:jingoh.org #:jingoh.examiner)
  (:import-from :cl-cpus #:get-number-of-processors)
  (:import-from :lparallel
                #:pmap
                #:psome
                #:pmapcan
                #:premove
                #:make-kernel
                #:*kernel*)
  (:import-from :jingoh.tester #:check #:*print-vivid*)
  (:import-from :jingoh.org #:spec-requirements #:spec-subject)
  (:import-from :jingoh.examiner #:print-dot #:print-summary #:break-on-finish)
  (:export ;;;; main api
           #:pexamine))

(in-package #:jingoh.parallel)

(defun pexamine
       (org
        &key subject ((:verbose *verbose*) *verbose*)
        ((:vivid *print-vivid*) *print-vivid*)
        (cores (get-number-of-processors)))
  (prog* ((*org*
           (handler-case (find-org org)
             (missing-org (c)
               (error
                 (make-condition 'missing-org
                                 :api 'examine
                                 :datum (datum c))))))
          (*package* (org-package *org*)) (*print-circle* t)
          (*kernel* (make-kernel cores)))
    (setf *issues*
            (handler-case
                (if (find *on-fails* '(:error :stop) :test #'eq)
                    (xxx-on-fails subject)
                    (print-progress subject))
              (missing-subject (c)
                (error
                  (make-condition 'missing-subject
                                  :api 'examine
                                  :datum (datum c))))))
    (print-summary *issues*)
    (when (or (<= 1 *verbose*) (eq :stop *on-fails*))
      (mapc #'print *issues*)))
  (terpri)
  (when *break-on-finish*
    (break-on-finish *issues*)))

(macrolet ((?! (form)
             `(or ,form
                  (error 'missing-subject
                         :api 'parallel-check
                         :datum subject))))
  (defun appropriate-specs (subject)
    (let ((specs (org-specifications *org*)))
      (case subject
        ((nil) specs) ; all
        ((t)
         (loop :for sub :in (org-current-subjects *org*) ; current
               :collect (find sub specs :key #'spec-subject)))
        (otherwise (list (?! (find subject specs :key #'spec-subject))))))))

(defun print-progress (subject)
  (pmapcan
    (lambda (spec)
      (let* ((result (pmap 'list #'check (spec-requirements spec)))
             (subject (spec-subject spec))
             (lock (bt:make-lock (symbol-name subject))))
        (when (<= 2 *verbose*)
          (bt:with-lock-held (lock)
            (format t "~&~S" subject)
            (mapc #'print-dot result)))
        (premove nil result)))
    (appropriate-specs subject)))

(defun xxx-on-fails (subject)
  (psome (lambda (spec) (psome #'check (spec-requirements spec)))
         (appropriate-specs subject)))