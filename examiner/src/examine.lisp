(defpackage :jingoh.examiner
  (:use :cl :jingoh.org :jingoh.tester :resignal-bind)
  (:export ;;;; main api
           #:examine
           ;;;; variables
           #:*verbose*
           #:*on-fails*
           #:*break-on-finish*
           #:*issues*
           ;;;; DSL
           #:with-examiner-context))

(in-package :jingoh.examiner) ; subject, detail, summary

(defparameter *verbose* 2 "Controls VERIFY's verbosity.")

(declaim (type (mod 4) *verbose*))

(defparameter *on-fails* nil)

(declaim (type (member :error :stop nil) *on-fails*))

(defparameter *break-on-finish* nil "Breaks when finish examine.")

(declaim (type boolean *break-on-finish*))

(defparameter *issues* nil "Previous issues. Debug use.")

(defparameter *requirement-form* nil "Previous test form. Debug use.")

(defmacro with-examiner-context (form)
  `(let ((*print-circle* t)
         (*print-length* nil) ; for sbcl at least.
         (*print-level* nil) ; for sbcl at least.
         (*print-pretty* t) ; for ccl at least.
         )
     ,form))

(define-condition break-on-fails (error) ())

(defun break-on-fails (result)
  (with-examiner-context (print result))
  (invoke-debugger (make-condition 'break-on-fails)))

(define-condition break-on-finish (error)
  ((issues :initarg :issues :reader issues))
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream "Break cause *BREAK-ON-FINISH*."))))

(defun break-on-finish (&optional issues)
  (invoke-debugger (make-condition 'break-on-finish :issues issues)))

(defmethod print-object ((c break-on-finish) stream)
  (if *print-escape*
      (print-unreadable-object (c stream)
        (princ
          (string-trim `(#\Newline)
                       (with-output-to-string (out)
                         (print-summary (issues c) out)))
          stream))
      (call-next-method)))

(defun print-progress (subject &optional (goto #'identity))
  (let ((current-subject '#:dummy) (issues))
    (do-requirements ((requirement sub) subject)
      (let ((result (check requirement)))
        (push result issues)
        (when result
          (setf *requirement-form* (requirement-form requirement))
          (case *on-fails*
            ((:error)
             (setf *issues* (apply #'nconc (nreverse issues)))
             (break-on-fails result))
            ((:stop)
             (setf *issues* (apply #'nconc (nreverse issues)))
             (format t "~2%; Stop to examine cause *ON-FAILS* at ~A~%" sub)
             (funcall goto))))
        (when (<= 2 *verbose*)
          (unless (eq sub current-subject)
            (setf current-subject sub)
            (format t "~V%~S"
                    (if (<= 3 *verbose*)
                        2
                        1)
                    current-subject))
          (if (<= 3 *verbose*)
              (print-requirement result requirement)
              (print-dot result)))))
    (apply #'nconc (nreverse issues))))

(defun print-requirement (result requirement)
  (let ((cl-ansi-text:*enabled* *print-vivid*) (token))
    (uiop:format! t "~&~A ~A"
                  (if result
                      (cl-ansi-text:red (setf token "Fails"))
                      (cl-ansi-text:green (setf token "Pass")))
                  (string-left-trim '(#\Space)
                                    (format nil "~VT ~S" (length token)
                                            requirement)))))

(defun print-dot (result)
  (let ((cl-ansi-text:*enabled* *print-vivid*))
    (if result
        (cl-ansi-text:with-color (:red)
          (write-char #\!))
        (cl-ansi-text:with-color (:green)
          (write-char #\.))))
  (force-output))

(defun print-summary (issues &optional (*standard-output* *standard-output*))
  (if (zerop (org-requirements-count *org*))
      (warn "No requirements in ~S" (org-name *org*))
      (let ((count (length issues)) (cl-ansi-text:*enabled* *print-vivid*))
        (if (zerop count)
            (format t "~&~A ~S" (cl-ansi-text:green "Pass") (org-name *org*))
            (format t "~&~A in ~S"
                    (cl-ansi-text:red (format nil "~D fail~:*~P" count))
                    (org-name *org*))))))

(defun examine
       (org
        &key subject ((:on-fails *on-fails*) *on-fails*)
        ((:verbose *verbose*) *verbose*)
        ((:vivid *print-vivid*) *print-vivid*))
  (setf *issues* nil)
  (prog* ((*org*
           (resignal-bind ((missing-org () 'missing-org :api 'examine))
             (find-org org)))
          (*package* (org-package *org*)))
    ;; in order to be able to see tag, we need SETF in PROG*'s body.
    (setf *issues*
            (resignal-bind ((missing-subject () 'missing-subject :api 'examine))
              (print-progress subject (lambda () (go #0=#:end)))))
    (print-summary *issues*)
   #0#
    (when (or (<= 1 *verbose*) (eq :stop *on-fails*))
      (with-examiner-context (mapc #'print *issues*))))
  (terpri)
  (when *break-on-finish*
    (break-on-finish *issues*)))
