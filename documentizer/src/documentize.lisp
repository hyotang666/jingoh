(in-package :jingoh.documentizer)

(declaim (optimize speed))

;;;; UTILITIES

(declaim
 (ftype (function
         (list ; of-type symbol
               )
         (values list ; of-type character
                 &optional))
        index-chars))

(defun index-chars (symbols)
  (sort (delete-duplicates (mapcar #'first-char symbols)) #'char<))

(declaim (ftype (function (symbol) (values character &optional)) first-char))

(defun first-char (symbol)
  (if (string= "" symbol)
      (error "Could not get first character from ~S" symbol)
      (locally ; due to unknown as base-char.
       (declare (optimize (speed 1)))
       (char-upcase (char (symbol-name symbol) 0)))))

(defmacro with-doc-directory ((pathname) &body body)
  `(with-output-to (,pathname)
     (let ((3bmd-code-blocks:*code-blocks* t))
       (3bmd:parse-string-and-print-to-stream
         (with-output-to-string (*standard-output*) ,@body)
         *standard-output*))))

;;;; VARIABLE for debug use.

(defvar *meta* nil)

;;;; DOCUMENTIZE

(defun documentize (system)
  (let* ((system (asdf:find-system system))
         (sys-dir (asdf:system-source-directory system))
         (meta-datas (meta-datas<=system system sys-dir)))
    (when meta-datas
      (setf *meta* meta-datas) ; for debug use.
      (let ((*default-pathname-defaults* (merge-pathnames "docs/" sys-dir)))
        (ensure-directories-exist *default-pathname-defaults*)
        (with-doc-directory ((merge-pathnames "top.html"))
          (top system))
        (with-doc-directory ((merge-pathnames "packages.html"))
          (packages meta-datas))
        (with-doc-directory ((merge-pathnames (target-path "symbols")))
          (symbol-index meta-datas system))
        (dolist (m meta-datas)
          (with-doc-directory ((merge-pathnames
                                 (format nil "P_~A.html" (meta-data-name m))))
            (about-package m))
          (about-symbols m #'section-callback))
        (table meta-datas #'table-callback)
        *default-pathname-defaults*))))

;;; GENERATORS

(declaim
 (ftype (function * (values null &optional))
        top
        packages
        symbol-index
        about-package))

;;; TOP

(defun top (system)
  (format t "# ~A~%~@[## ~A~%~]~@[~A~2%~]~{~D. [~A](~A)~&~}"
          (asdf:coerce-name system) (asdf:system-description system)
          (asdf:system-long-description system)
          (loop :for i :of-type (mod #.most-positive-fixnum) :upfrom 1
                :for title :in '(packages symbols)
                :collect i
                :collect (symbol-name title)
                :collect (target-path (string-downcase (symbol-name title))))))

;;; PACKAGES

(defun packages (meta-datas)
  #+allegro
  (check-type meta-datas list)
  (format t "# Packages Index~%")
  (loop :for i :of-type (mod #.most-positive-fixnum) :upfrom 1
        :for m :in meta-datas
        :do (format t "~D. [~A](~A)~%" i (string (meta-data-name m))
                    (target-path (format nil "P_~A" (meta-data-name m))))))

;;; SYMBOL-INDEX

(defun symbol-index (meta-datas system)
  (labels ((links (chars &optional (code #.(char-code #\A)) have-non-alph-p acc)
             (let (char)
               (cond
                 ((not (<= code #.(char-code #\Z)))
                  (nreconc acc (or have-non-alph-p '("Non-Alphabetic"))))
                 ((null (setq char (car chars)))
                  (links chars (1+ code) have-non-alph-p
                         (cons (princ-to-string (code-char code)) acc)))
                 ((not (alpha-char-p char))
                  (links (cdr chars) code
                         (adjoin
                           (format nil "[Non-Alphabetic](~A)"
                                   *x-non-alph-namestring*)
                           have-non-alph-p
                           :test #'string=)
                         acc))
                 ((= code (char-code char))
                  (links (cdr chars) (1+ code) have-non-alph-p
                         (cons
                           (format nil "[~A](~A)" char (x-alph-pathname char))
                           acc)))
                 (t
                  (links chars (1+ code) have-non-alph-p
                         (cons (princ-to-string (code-char code)) acc)))))))
    (declare
      (ftype (function (list &optional (mod #.most-positive-fixnum) list list)
              (values list &optional))
             links))
    (let* ((symbols
            (uiop:while-collecting (collect)
              (dolist (m meta-datas)
                (dospecifieds (symbol m)
                  (collect symbol)))))
           (num-symbols (length symbols))
           (index-chars (index-chars symbols)))
      (declare (type list symbols))
      (funcall
        (formatter
         "# Alphabetical Symbol Index~2%There ~:[is~;are~] ~D symbol~P by ~A.~2%~{~A~^ | ~}")
        *standard-output* (not (= 1 num-symbols)) num-symbols num-symbols
        (asdf:coerce-name system) (links index-chars)))))

;;; ABOUT-PACKAGE

(defun about-package (meta-data)
  (format t
          (locally ; due to ~:*
           (declare (optimize (speed 1)))
           (formatter "# ~A~%~@[## ~:*~A Concepts~%~A~%~]## ~A Dictionary~2%"))
          (meta-data-name meta-data) (meta-data-doc meta-data)
          (meta-data-name meta-data))
  (flet ((find-in-sec (symbol section)
           (donames (name section)
             (declare (type symbol name))
             (when (string= symbol name)
               (return t)))))
    (declare
      (ftype (function (symbol section) (values boolean &optional))
             find-in-sec))
    (doexports (symbol meta-data)
      (format t "* ~A~&"
              (let ((sec
                     (dosections (section meta-data)
                       (when (find-in-sec symbol section)
                         (return section)))))
                (if sec
                    (format nil "[~A](~A)" (escape-* symbol)
                            (section-path sec))
                    (escape-* symbol)))))))

;;; ABOUT-SYMBOLS

(defun about-symbols
       (meta-data
        &optional (callback #'princ)
        &aux (callback (coerce callback 'function)))
  (dosections (section meta-data)
    (funcall callback section)))

(defun section-callback (section)
  (with-doc-directory ((merge-pathnames (section-path section)))
    (princ section)))

;;; TABLE

(defun table
       (meta-datas
        &optional (callback #'table-printer)
        &aux (callback (coerce callback 'function)))
  (labels ((line (sec)
             (uiop:while-collecting (acc)
               (donames (name sec)
                 (acc
                   (cons name
                         (format nil "[~A](~A)~%" (escape-* name)
                                 (section-path sec))))))))
    (let ((pairs
           (loop :for sec
                      :in (uiop:while-collecting (collect)
                            (dolist (m meta-datas)
                              (dosections (s m)
                                (collect s))))
                 :nconc (line sec) :into result
                 :finally (return (sort result #'string< :key #'car))))
          (index-chars
           (index-chars
             (uiop:while-collecting (collect)
               (dolist (m meta-datas)
                 (dospecifieds (symbol m)
                   (collect symbol)))))))
      (labels ((rec (chars pairs)
                 (unless (endp chars)
                   (apply #'rec (funcall callback chars pairs)))))
        (rec index-chars pairs)))))

(declaim
 (ftype (function (list list) (values (cons list (cons list null)) &optional))
        table-callback
        table-printer))

(defun table-callback (chars pairs)
  (let ((char (car chars)) return)
    (with-doc-directory ((if (alpha-char-p char)
                             (x-alph-pathname char)
                             *x-non-alph-namestring*))
      (setf return (table-printer chars pairs)))
    return))

(defun table-printer (chars pairs)
  (if (endp pairs)
      (list nil nil)
      (if (char= (car chars) (first-char (caar pairs)))
          (progn
           #1=(format t "* ~A~&" (cdar pairs))
           (table-printer chars (cdr pairs))) ; cdring pairs.
          (if (alpha-char-p (car chars))
              #0=(list (cdr chars) pairs) ; cdring chars.
              (if (alpha-char-p (first-char (caar pairs)))
                  #0# ; cdring chars.
                  (progn #1# (table-printer (cdr chars) (cdr pairs)))))))) ; cdring pairs and chars.

