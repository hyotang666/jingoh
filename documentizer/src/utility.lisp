(in-package :jingoh.documentizer)

(declaim (optimize speed))

(declaim
 (ftype (function ((or symbol simple-string)) (values simple-string &optional))
        escape-*))

(defun escape-* (arg)
  (with-output-to-string (*standard-output*)
    (loop :for c
               :across (etypecase arg
                         (symbol (prin1-to-string arg))
                         (string arg))
          :when (char= #\* c)
            :do (write-char #\\)
                (write-char c)
          :else
            :do (write-char c))))

(defun x-alph-pathname (char) (target-path (format nil "X_Alph_~A" char)))

(defvar *x-non-alph-namestring* "X_NonAlpha.html")

(defvar *target-type* "html")

(defun target-path (name)
  #+(or allegro abcl)
  (check-type name string)
  (make-pathname :name name :type *target-type*))

(defmacro with-output-to ((pathname) &body body)
  `(with-open-file (*standard-output* ,pathname :direction :output
                    :if-exists :supersede
                    :if-does-not-exist :create)
     ,@body))