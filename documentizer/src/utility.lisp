(in-package :jingoh.documentizer)

(declaim (optimize speed))

(defun escape-* (arg)
  (flet ((ensure-symbol-notation (arg)
           (declare (optimize (speed 1))) ; due to type uncertainty.
           (if (uiop:string-suffix-p (prin1-to-string arg) "|")
               (format nil "|~A|" arg)
               (string arg))))
    (declare
      (ftype (function ((or symbol string)) (values simple-string &optional))
             ensure-symbol-notation))
    (with-output-to-string (*standard-output*)
      (loop :for c :across (ensure-symbol-notation arg)
            :when (char= #\* c)
              :do (write-char #\\)
                  (write-char c)
            :else
              :do (write-char c)))))

(defun x-alph-pathname (char) (target-path (format nil "X_Alph_~A" char)))

(defvar *x-non-alph-namestring* "X_NonAlpha.html")

(defvar *target-type* "html")

(defun target-path (name) (make-pathname :name name :type *target-type*))

(defmacro with-output-to ((pathname) &body body)
  `(with-open-file (*standard-output* ,pathname :direction :output
                    :if-exists :supersede
                    :if-does-not-exist :create)
     ,@body))