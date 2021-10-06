(in-package :jingoh.documentizer)

(declaim (optimize speed))

(defun parse-spec (pathname)
  (when (probe-file pathname)
    (multiple-value-call #'engroup (sectionize (enlist pathname)))))

(declaim
 (ftype (function (pathname)
         (values list ; of simple-string
                 &optional))
        enlist))

(defun enlist (pathname)
  (with-open-file (s pathname)
    (loop :for exp
               = (let ((read-as-string:*muffle-reader-error* t))
                   (read-as-string:read-as-string s nil nil))
          :while exp
          :collect exp)))

(declaim
 (ftype (function
         (list ; of-type simple-string
               )
         (values list ; of-type single
                 list ; of-type common
                 &optional))
        sectionize))

(defun sectionize (list)
  (loop :for list :on list
        :with body
        :for section = (section-p (car list))
        :for alias = (getf section :as)
        :if section
          :do (setf body
                      (subseq list 1 (position-if #'section-p list :start 1)))
          :and :if alias
                 :collect (make-common :names (cadr section)
                                       :alias alias
                                       :doc-type (getf section
                                                       :doc-type :unbound)
                                       :path (target-path
                                               (symbol-name (gensym "C_")))
                                       :body body)
                   :into commons
               :else
                 :collect (make-single :name (cadr section)
                                       :doc-type (getf section
                                                       :doc-type :unbound)
                                       :path (target-path
                                               (format nil "S_~A"
                                                       (replace-invalid-chars
                                                         (cadr section))))
                                       :body body)
                   :into singles
        :finally (return (values singles commons))))

(declaim (ftype (function (simple-string) (values list &optional)) section-p))

(defun section-p (elt)
  (and (uiop:string-prefix-p "(" elt)
       (let* ((*package* (find-package :jingoh.documentizer))
              (null-package:*only-junk-p* t)
              (sexp
               (with-input-from-string (s elt)
                 (null-package:read-with-null-package s))))
         (when (find (car sexp)
                     '(requirements-about common-requirements-about))
           sexp))))

(defun replace-invalid-chars (arg)
  (with-output-to-string (*standard-output*)
    (loop :for c
               :across (string-downcase
                         (locally ; due to type uncertainty.
                          (declare (optimize (speed 1)))
                          (string arg)))
          :when (and (not (alphanumericp c)) (not (char= #\. c)))
            :do (write (char-code c))
          :else
            :do (write-char c))))

(declaim
 (ftype (function
         (list ; of-type single
               list ; of-type common
               )
         (values list ; of-type section
                 &optional))
        engroup))

(defun engroup (singles commons)
  (nconc commons
         (loop :for single :in singles
               :for common
                    = (find (car (section-names single)) commons
                            :key #'section-names
                            :test (lambda (name list)
                                    (declare (type list list)
                                             (type symbol name))
                                    (find name list :test #'string=)))
               :if common
                 :do (setf (section-body common)
                             (nconc (section-body common)
                                    (cons
                                      (format nil "#| ~A |#"
                                              (car (section-names single)))
                                      (section-body single))))
               :else
                 :collect single)))