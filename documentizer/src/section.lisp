(in-package :jingoh.documentizer)

(defstruct (section (:predicate nil)) body path names (doc-type :unbound))

(defstruct (single (:include section)
                   (:constructor make-single
                    (&key body path name doc-type &aux (names (list name))))))

(defstruct (common (:include section)) alias)

(defmethod print-object ((obj single) *standard-output*)
  (if *print-escape*
      (print-unreadable-object (obj *standard-output* :type nil :identity nil)
        (prin1 (car (section-names obj))))
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
       (format t "# ~{~A~^, ~}~&" (mapcar #'escape-* (section-names obj)))
       (princ-section-body (section-body obj)))))

(defparameter *print-example* t)

(declaim (type (or null (integer 0 *)) *tab-expand*))

(defparameter *tab-expand* 8)

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
       (loop :with ops = 0
             :for (key value) :on (cdddr list) :by #'cddr
             :while (uiop:string-prefix-p "," key)
             :do (when *print-example*
                   (format t "~A ~A~%" key value))
                 (incf ops)
             :finally (when *print-example*
                        (format t "```~%"))
                      (setf list (nthcdr (+ 2 (* 2 ops)) list))))
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
       (write-string "# ")
       (write-line elt *standard-output* :start 3 :end (- (length elt) 2)))
      (t nil))))