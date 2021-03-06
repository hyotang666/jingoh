(in-package :jingoh.tester)

(defstruct issue form expected actual line)

(defstruct (test-issue (:include issue)) test)

(defstruct (condition-issue (:include issue)) message)

(defstruct (jingoh-internal-issue (:include condition-issue)))

(defstruct (error-was-signaled (:include condition-issue)))

(defstruct (warning-was-signaled (:include condition-issue)))

(defstruct (debugger-was-invoked (:include condition-issue)))

(defstruct (unmatch-condition (:include condition-issue)))

(defstruct (unexpected-success (:include issue)))

(defstruct (unexpected-output (:include issue)))

(defstruct (missing-restarts (:include issue)))

(defstruct (unsatisfied-clause (:include issue)) args)

(defstruct (wrong-format (:include test-issue)))

(defstruct (issue-of-multiple-values (:include test-issue)))

(defparameter *print-vivid* t)

(defmethod print-object ((issue issue) stream)
  (if (null *print-vivid*)
      (call-next-method)
      (let ((string
             (with-output-to-string (stream)
               (let ((i (copy-structure issue)))
                 (when (should-print-vivid-p i)
                   (setf (issue-actual i)
                           (mismatch-sexp (issue-actual issue)
                                          (issue-expected issue))))
                 (call-next-method i stream)))))
        (princ (regex-replace issue string) stream))))

(defmethod print-object ((issue unsatisfied-clause) stream)
  (declare (ignore stream))
  (if (not *print-vivid*)
      (call-next-method)
      (progn
       (when (typep (issue-form issue) '(cons (member eq eql equal equalp) *))
         (let ((args (unsatisfied-clause-args issue)))
           (setf (unsatisfied-clause-args issue)
                   (list (car args) (mismatch-sexp (cadr args) (car args))))))
       (call-next-method))))

(defun should-print-vivid-p (issue)
  (and (not
         (typep issue
                '(or condition-issue
                     unexpected-success
                     unexpected-output
                     missing-restarts)))
       (or (typep (issue-expected issue) '(or sequence pathname array))
           (typep (class-of (issue-expected issue)) 'structure-class))))

(defun regex-replace (issue string)
  (labels ((rec (post slots acc)
             (if (endp slots)
                 (do-return post acc)
                 (body post
                       (symbol-name (c2mop:slot-definition-name (car slots)))
                       (cdr slots) #'cl-ansi-text:yellow acc)))
           (body (string name rest color &optional acc)
             (destructuring-bind
                 (pre post)
                 (ppcre:split name string :limit 2)
               (rec post rest
                    (list*
                      (let ((*print-circle* nil)) ; <--- sbcl needs.
                        (funcall color name))
                      pre acc))))
           (do-return (string acc)
             (apply #'concatenate 'string (nreverse (push string acc)))))
    (body string (symbol-name (type-of issue))
          (c2mop:class-slots (class-of issue)) #'cl-ansi-text:red)))

(defstruct (diff (:constructor markup (object))) object)

(defstruct (diff-comparable (:include diff)) origin)

(defstruct (string-diff (:include diff-comparable)
                        (:constructor markup-string (object origin))))

(defstruct (pathname-diff (:include diff-comparable)
                          (:constructor markup-pathname (object origin))))

(defvar *color-hook* #'cl-ansi-text:red)

(defmethod print-object ((object diff) *standard-output*)
  (if *print-vivid*
      (princ
        (let ((string (prin1-to-string (diff-object object)))
              (*print-circle* nil)) ; <--- sbcl needs.
          (funcall *color-hook* string)))
      (prin1 (diff-object object))))

(defmethod print-object ((object string-diff) *standard-output*)
  (if (null *print-vivid*)
      (prin1 (diff-object object))
      (let* ((pos
              (mismatch (string-diff-origin object)
                        (string-diff-object object)))
             (expected-in-bounds-p
              (array-in-bounds-p (string-diff-origin object) pos))
             (actual-in-bounds-p
              (array-in-bounds-p (string-diff-object object) pos)))
        (if expected-in-bounds-p
            (if actual-in-bounds-p
                ;; simply different. e.g. "foobar" "foohoge"
                #0=(progn
                    (princ #\")
                    (write-string (string-diff-object object) nil :end pos)
                    (write-string
                      (funcall
                        (lambda (string)
                          (funcall *color-hook* string :style :background))
                        (subseq (string-diff-object object) pos)))
                    (princ #\"))
                ;; too much short. e.g. "foobar" "foo"
                (progn
                 (princ #\")
                 (write-string (string-diff-object object))
                 (write-string (funcall *color-hook* ":NULL"))
                 (princ #\")))
            (if actual-in-bounds-p
                ;; too much long. e.g. "foo" "foobar"
                #0#
                ;; simply different e.g. "foo" "bar"
                (prin1 (funcall *color-hook* (string-diff-object object))))))))

(defmethod print-object ((object pathname-diff) *standard-output*)
  (let ((diff (pathname-diff-object object))
        (origin (pathname-diff-origin object)))
    (labels ((diff? (a b &key (test #'eql))
               (if (funcall test a b)
                   a
                   (etypecase a
                     (symbol (markup a))
                     (string (markup-string a b)))))
             (rec (a b &optional acc)
               (if a
                   (if b
                       (rec (cdr a) (cdr b)
                            (push (diff? (car a) (car b) :test #'equal) acc))
                       (nreconc acc
                                (loop :for elt :in a
                                      :collect (funcall *color-hook*
                                                        (string elt)))))
                   (if b
                       (nreconc acc (funcall *color-hook* ":NULL"))
                       (nreverse acc)))))
      (if (null *print-vivid*)
          (prin1 diff)
          (format t "#P~S"
                  `(:host ,(diff? (pathname-host diff) (pathname-host origin))
                    :device
                    ,(diff? (pathname-device diff) (pathname-device origin))
                    :directory
                    ,(rec (pathname-directory diff)
                          (pathname-directory origin))
                    :name
                    ,(diff? (pathname-name diff) (pathname-name origin)
                            :test #'equal)
                    :type
                    ,(diff? (pathname-type diff) (pathname-type origin)
                            :test #'equal)
                    :version
                    ,(diff? (pathname-version diff)
                            (pathname-version origin))))))))

(defun mismatch-sexp (actual expected)
  (let (env)
    (labels ((rec (actual expected)
               (typecase expected
                 ((cons (eql quote) t)
                  (if (typep actual '(cons (eql quote) t))
                      (cons 'quote (rec (cdr actual) (cdr expected)))
                      (markup actual)))
                 (cons
                  (if (typep actual '(or atom (cons (eql quote) t)))
                      (markup actual)
                      (cons (rec (car actual) (car expected))
                            (rec (cdr actual) (cdr expected)))))
                 ((or boolean keyword)
                  (if (eq expected actual)
                      actual
                      (markup actual)))
                 (symbol
                  (if (not (symbolp actual))
                      (markup actual)
                      (if (symbol-package actual)
                          (if (eq expected actual)
                              actual
                              (markup actual))
                          ;; actual is uninterned symbol.
                          (let ((pair (assoc actual env :test #'eq)))
                            (if pair ; seen.
                                (if (eq expected (cdr pair))
                                    actual
                                    (markup actual)) ; unseen.
                                (let ((pair (rassoc expected env :test #'eq)))
                                  (if pair ; seen.
                                      (markup actual)
                                      (progn
                                       (push (cons actual expected) env)
                                       actual))))))))
                 (string
                  (if (not (stringp actual))
                      (markup actual)
                      (if (string= expected actual)
                          actual
                          (markup-string actual expected))))
                 (pathname
                  (if (not (pathnamep actual))
                      (markup actual)
                      (if (equal expected actual)
                          actual
                          (markup-pathname actual expected))))
                 ((or number character bit-vector)
                  (if (equal expected actual)
                      actual
                      (markup actual)))
                 (vector
                  (if (not (vectorp actual))
                      (markup actual)
                      (do* ((i 0 (1+ i))
                            (a-p (array-in-bounds-p expected i)
                                 (array-in-bounds-p expected i))
                            (b-p (array-in-bounds-p actual i)
                                 (array-in-bounds-p actual i))
                            (acc))
                           ((or (and (not a-p) (not b-p)) ; same length
                                (and (not a-p) b-p) ; actual is longer
                                (and a-p (not b-p))) ; actual is shorter
                            (cond
                              ((and (not a-p) (not b-p)) ; same length
                               (coerce (nreverse acc) 'vector))
                              ((and (not a-p) b-p) ; actual is longer
                               (concatenate 'vector (nreverse acc)
                                            (map 'vector #'markup
                                                 (subseq actual i))))
                              ((and a-p (not b-p)) ; actual is shorter
                               (coerce (nreverse (cons :null acc)) 'vector))))
                        (push (rec (aref actual i) (aref expected i)) acc))))
                 (array
                  (if (not (arrayp actual))
                      (markup actual)
                      (if (not
                            (equal (array-dimensions expected)
                                   (array-dimensions actual)))
                          (markup
                            (list :different-dimensions :expected
                                  (array-dimensions expected) :actual
                                  (array-dimensions actual) actual))
                          (let ((a (make-array (array-dimensions actual))))
                            (dotimes (i (array-total-size expected) a)
                              (setf (row-major-aref a i)
                                      (rec (row-major-aref actual i)
                                           (row-major-aref expected i))))))))
                 (t
                  (if (not (typep (class-of expected) 'structure-class))
                      (if (equal expected actual)
                          actual
                          (markup actual))
                      (if (not (eq (type-of expected) (type-of actual)))
                          (markup actual)
                          (loop :with object = (copy-structure actual)
                                :for slot1 :in (slots<=obj expected)
                                :for slot2 :in (slots<=obj actual)
                                :do (setf (slot-value object slot2)
                                            (rec (slot-value actual slot2)
                                                 (slot-value expected slot1)))
                                :finally (return object))))))))
      (rec actual expected))))

(defun slots<=obj (obj)
  (mapcar #'closer-mop:slot-definition-name
          (closer-mop:class-slots (class-of obj))))