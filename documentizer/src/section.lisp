(in-package :jingoh.documentizer)

(declaim (optimize speed))

;;; OBJECT

(defstruct (section (:predicate nil))
  (body nil :type list)
  (path (error "PATH is required.") :type pathname :read-only t)
  (names nil :type list :read-only t)
  (doc-type :unbound
            :type (member :unbound nil t function compiler-macro setf
                          method-combination type structure variable)))

(defstruct (single (:include section)
                   (:constructor make-single
                    (&key body path name doc-type &aux (names (list name))))))

(defstruct (common (:include section))
  (alias (error "ALIAS is required.") :type symbol :read-only t))

;;; ABSTRACTION BARRIARS

(defmacro donames ((var <section> &optional <return>) &body body)
  (setf var (uiop:ensure-list var)) ; canonicalize.
  `(loop :for ,var :on (section-names ,<section>)
         :do (let ((,(car var) ,(car var)))
               ,@body)
         :finally (return ,<return>)))

(defun conc-body (section contents)
  (setf (section-body section) (nconc (section-body section) contents)))

(defun list<-body (section) (section-body section))

(defun single-name (single) (car (single-names single)))

;;; PRINTERS

(defmethod print-object ((obj single) *standard-output*)
  (if *print-escape*
      (print-unreadable-object (obj *standard-output* :type nil :identity nil)
        (prin1 (single-name obj)))
      (call-next-method)))

(defmethod print-object ((obj common) *standard-output*)
  (if *print-escape*
      (print-unreadable-object (obj *standard-output* :type nil :identity nil)
        (prin1 (common-alias obj)))
      (call-next-method)))

(defmethod print-object ((obj section) *standard-output*)
  (if *print-escape*
      (call-next-method)
      (progn
       (write-string "# ")
       (donames ((name . rest?) obj (fresh-line))
         (write-string (escape-* name))
         (when rest?
           (write-string ", ")))
       (princ-section-body (list<-body obj)))))

(defparameter *print-example* t)

(declaim (type (or null (integer 0 *)) *tab-expand*))

(defparameter *tab-expand* 8)

(declaim
 (ftype (function ((or null simple-string))
         (values (or null simple-string) &optional))
        expand-tab))

(defun expand-tab (line)
  (when (and line *tab-expand*)
    (values (ppcre:regex-replace-all #\Tab line
                                     (make-string *tab-expand*
                                                  :initial-element #\Space)))))

(defun princ-section-body (body)
  (do* ((list body (cdr list))
        (elt #0=(expand-tab (car list)) #0#))
       ((endp list) (force-output))
    (cond
      ((uiop:string-prefix-p ";;;; " elt)
       (format t "~%## ")
       (write-string elt *standard-output* :start 5))
      ((uiop:string-prefix-p ";;; " elt)
       (format t "~%### ")
       (write-line elt *standard-output* :start 4))
      ((uiop:string-prefix-p ";; " elt)
       (format t "~%#### ")
       (write-line elt *standard-output* :start 3))
      ((uiop:string-prefix-p "; " elt)
       (write-string elt *standard-output* :start 2))
      ((uiop:string-prefix-p "#?" elt)
       (when *print-example*
         (format t "~%```lisp~%~A~%~A ~A~%" elt (cadr list) (caddr list)))
       (loop :with ops :of-type (mod #.most-positive-fixnum) = 0
             :for (key value) :on (cdddr list) :by #'cddr
             :while (uiop:string-prefix-p "," key)
             :do (when *print-example*
                   (format t "~A ~A~%" key value))
                 (incf ops)
             :finally (when *print-example*
                        (format t "```~%"))
                      (setf list
                              (nthcdr
                                (the (mod #.most-positive-fixnum)
                                     (+ 2 (* 2 ops)))
                                list))))
      ((uiop:string-prefix-p "#+syntax" elt)
       (format t "~%### ")
       (write-line (escape-* elt) *standard-output* :start 2))
      ((uiop:string-prefix-p "#+setf" elt)
       (write-string "### ")
       (write-line (escape-* elt) *standard-output* :start 2))
      ((uiop:string-prefix-p "#+signature" elt)
       (write-string "* ")
       (write-line (escape-* elt) *standard-output* :start 12))
      ((uiop:string-prefix-p "#| " elt)
       (locally
        (declare (type simple-string elt))
        (write-string "# ")
        (write-line elt *standard-output* :start 3 :end (- (length elt) 2))))
      (t nil))))