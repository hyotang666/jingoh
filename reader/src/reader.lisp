(in-package :cl-user)

(defpackage :jingoh.reader
  (:use :cl)
  (:export ;;;; main api
           #:enable
           #:replace-macro-character
           #:|#?reader|
           ;;;; special symbol for debug/trace
           #:*read-verbose*
           #:*read-print*
           ;;;; readtable name
           #:syntax
           ;;;; conditions
           #:macro-char-confliction))

(in-package :jingoh.reader)

(declaim (optimize speed))

(defparameter *dispatch-macro-character* #\#)

(defparameter *dispatch-macro-sub-char* #\?)

(defmacro enable (&optional (char '*dispatch-macro-sub-char*))
  (let ((var (gensym "CHAR")) (it (gensym "TEMP")))
    `(let* ((,var ,char)
            (,it
             (named-readtables::%get-dispatch-macro-character
               *dispatch-macro-character* ,var *readtable*)))
       (if (null ,it) ; Noone use it.
           #0=(replace-macro-character *dispatch-macro-character* ,var)
           (if (eq '|#?reader| ,it) ; it's me!
               #0#
               (restart-case (error 'macro-char-confliction
                                    :format-control "Dispatch macro #~C is used. ~S"
                                    :format-arguments (list ,var ,it))
                 (replace ()
                     :report "Replace it."
                   #0#)))))))

(defun replace-macro-character (char sub-char)
  (check-type char character)
  (check-type sub-char character)
  (setf *dispatch-macro-character* char
        *dispatch-macro-sub-char* sub-char)
  (set-dispatch-macro-character char sub-char '|#?reader|))

(define-condition macro-char-confliction (simple-error) ())

(defparameter *read-verbose* nil)

(defparameter *read-print* nil)

(defparameter *lines* nil)

(declaim (type (mod #.most-positive-fixnum) *line*))

(defvar *line*)

(defun |#?reader| (stream character number)
  (declare (ignore character))
  (setf *lines* (or *lines* (collect-spec-lines stream)))
  (let ((*line* 1))
    (|#?reader-body| stream number)))

(defvar *reader-package* :cl)

(declaim
 (ftype (function (stream (or null (mod #.most-positive-fixnum)))
         (values cons &optional))
        |#?reader-body|))

(defun |#?reader-body| (stream number)
  (labels ((read-form (as)
             (let ((form (uiop:symbol-call *reader-package* 'read stream t t t)))
               (when *read-print*
                 (funcall (formatter "~%~S: ~S") *trace-output* as form))
               form))
           (options ()
             (if number
                 (loop :repeat number
                       :collect (read-form '#:option-key)
                       :collect (read-form '#:option-value))
                 (loop :while (have-option?)
                       :collect (read-form '#:option-key)
                       :collect (read-form '#:option-value))))
           (have-option? ()
             (case
                 (handler-case
                     (uiop:symbol-call *reader-package* 'peek-char t stream nil
                                       nil t)
                   ;; cmucl signals.
                   (end-of-file ()))
               (#\Newline
                (|line-counter| stream (read-char stream))
                (have-option?))
               (#\, (read-char stream t t t))
               (#\;
                (|line-comment| stream (read-char stream))
                (have-option?)))))
    (let ((form
           `(jingoh.tester:defspec
              ,(read-form '#:test-form)
              ,(read-form '#:keyword)
              ,(read-form '#:expected)
              :line
              ,(pop *lines*)
              ,@(options))))
      (when (or *read-verbose* *read-print*)
        (funcall (formatter "~%READ: ~S") *trace-output* form))
      form)))

(locally
 #+sbcl ; out of our responsibility.
 (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
 (named-readtables:defreadtable syntax
   (:merge :standard)
   (:dispatch-macro-char *dispatch-macro-character* *dispatch-macro-sub-char*
    '|#?reader|)))

;;;; COLLECT-SPEC-LINES

(defvar *line-pos*)

(defun |line-counter| (stream character)
  (declare (ignore stream character))
  (incf *line*)
  (values))

(defun |#?counter| (stream character number)
  (declare (ignore character))
  (push *line* *line-pos*)
  (let ((*reader-package* :eclector.reader))
    (|#?reader-body| stream number)))

(defun |line-comment| (stream character)
  (declare (ignore character))
  (read-line stream)
  (incf *line*)
  (values))

(defun |block-comment| (stream character number)
  (declare (ignore character number))
  (loop :for char = (read-char stream)
        :do (case char
              (#\#
               (let ((char (peek-char nil stream t t t)))
                 (case char
                   (#\| ; nested comment.
                    (|block-comment| stream char nil))
                   ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
                    (loop :initially (read-char stream)
                          :for char = (peek-char nil stream)
                          :while (digit-char-p char)
                          :do (read-char stream))
                    (if (char= #\| (read-char stream)) ; nested comment with
                                                       ; number, e.g. #1|.
                        (|block-comment| stream #\| nil)))
                   (#\Newline (|line-counter| stream (read-char stream)))
                   (otherwise #|Do nothing, to next loop|#))))
              (#\Newline (|line-counter| stream char))
              (#\|
               (if (char= #\# (peek-char nil stream))
                   (progn (read-char stream) (loop-finish))
                   #|Do nothing, to next loop|#))
              (otherwise #|Do nothing, to next loop|#)))
  (values))

(defun |#+counter| (stream character number)
  (declare (ignore number))
  (if (funcall (ecase character (#\+ #'identity) (#\- #'not))
               (uiop:featurep
                 (let ((*package* (find-package :keyword)))
                   (eclector.reader:read stream t t t))))
      (eclector.reader:read stream t t t)
      (handler-bind ((error
                       (lambda (condition)
                         (let ((restart
                                (find-restart 'eclector.reader:recover
                                              condition)))
                           (when restart
                             (invoke-restart restart))))))
        (eclector.reader:read stream t t t))))

(let ((reader
       (coerce
         (load-time-value
          (eclector.readtable:get-macro-character
            eclector.readtable:*readtable* #\")
          t)
         'function)))
  #+cmu
  (declare (type function reader))
  (defun |string-line-counter| (stream character)
    (let ((string (funcall reader stream character)))
      (incf *line* (count #\Newline string))
      string)))

(defvar *counter*
  (eclector.readtable:copy-readtable eclector.readtable:*readtable*))

(flet ((syntax (type &rest args)
         (apply #'uiop:symbol-call :eclector.readtable
                (format nil "SET-~A-CHARACTER" type) *counter* args)))
  (syntax :macro #\Newline '|line-counter|)
  (syntax :macro #\; '|line-comment|)
  (syntax :macro #\" '|string-line-counter|)
  (syntax :dispatch-macro #\# #\+ '|#+counter|)
  (syntax :dispatch-macro #\# #\- '|#+counter|)
  (syntax :dispatch-macro #\# #\| '|block-comment|))

(defun %collect-spec-lines (input)
  ;; Split for easy to debug.
  (let ((*line-pos*)
        (*line* 1)
        (eclector.readtable:*readtable*
         (eclector.readtable:copy-readtable *counter*)))
    ;; To set dispatch macro dynamically, since it may be customized.
    (eclector.readtable:set-dispatch-macro-character
      eclector.readtable:*readtable* *dispatch-macro-character*
      *dispatch-macro-sub-char* '|#?counter|)
    (loop :with tag = '#:end
          :for sexp = (eclector.reader:read input nil tag)
          :until (eq sexp tag))
    (nreverse *line-pos*)))

(declaim (ftype (function (stream) (values list &optional)) collect-spec-lines))

(defun collect-spec-lines (input)
  (let ((pathname (ignore-errors (pathname input))))
    (when (and pathname (probe-file pathname)) ; ECL need.
      (with-open-file (s pathname :external-format (stream-external-format
                                                     input))
        (%collect-spec-lines s)))))