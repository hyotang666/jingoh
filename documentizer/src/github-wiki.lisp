(in-package :jingoh.documentizer)

;;;; UTILITIES

(defmacro with-open-markdown ((name) &body body)
  `(with-output-to ((make-pathname
                     :name ,name
                     :type "md"
                     :defaults *default-pathname-defaults*))
     ,@body))

;;;; GITHUB-WIKI

(defun github-wiki (system &optional (pathname (uiop:getcwd)))
  (let* ((system (asdf:find-system system))
         (*target-type* nil)
         (*x-non-alph-namestring* "X_NonAlpha")
         (*default-pathname-defaults* (pathname pathname))
         (meta-datas
          (meta-datas<=system system (asdf:system-source-directory system))))
    (when meta-datas
      (setf *meta* meta-datas) ; for debug use.
      (with-open-markdown ("home")
        (top system))
      (with-open-markdown ("packages")
        (packages meta-datas))
      (with-open-markdown ("symbols")
        (symbol-index meta-datas system))
      (dolist (m meta-datas)
        (with-open-markdown ((format nil "P_~A" (meta-data-name m)))
          (about-package m))
        (about-symbols m #'section-callback.md))
      (table meta-datas #'table-callback.md)
      *default-pathname-defaults*)))

(defun section-callback.md (section)
  (with-open-markdown ((namestring (section-path section)))
    (princ section)))

(defun table-callback.md (chars pairs)
  (let ((char (car chars)) return)
    (with-open-markdown ((if (alpha-char-p char)
                             (namestring (x-alph-pathname char))
                             *x-non-alph-namestring*))
      (setf return (table-printer chars pairs)))
    return))