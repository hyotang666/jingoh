(in-package :jingoh.tester)

(defstruct issue form expected actual line comment)

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

(defun vprint-issue (output issue)
  (vivid-colors:vprint-logical-block (output nil :prefix "#S(" :suffix ")")
    (vivid-colors:put (type-of issue) output :color cl-colors2:+red+) ; op
    (write-char #\Space output)
    (vivid-colors:vprint-newline :linear output)
    (loop :for (slot . rest) :on (c2mop:class-slots (class-of issue))
          :for name
               = #.(or #+abcl `(aref slot 1) `(c2mop:slot-definition-name slot))
          :do (vivid-colors:put name output
                                :color cl-colors2:+yellow+
                                :key (lambda (x) (format nil ":~A" x)))
              (write-char #\Space output)
              (if (eq 'actual name)
                  (vivid-colors:vprint
                    (vivid-diff:mismatch-sexp (issue-actual issue)
                                              (issue-expected issue))
                    output t)
                  (vivid-colors:vprint (slot-value issue name) output t))
              (when rest
                (write-char #\Space output)
                (vivid-colors:vprint-newline :linear output)))))

(defun vprint-unsatisfied-clause (output issue)
  (vivid-colors:vprint-logical-block (output nil :prefix "#S(" :suffix ")")
    (vivid-colors:put (type-of issue) output :color cl-colors2:+red+) ; op
    (write-char #\Space output)
    (vivid-colors:vprint-newline :linear output)
    (loop :for (slot . rest) :on (c2mop:class-slots (class-of issue))
          :for name
               := #.(or #+abcl
                        `(aref slot 1)
                        `(c2mop:slot-definition-name slot))
          :do (vivid-colors:put name output
                                :color cl-colors2:+yellow+
                                :key (lambda (x) (format nil ":~A" x)))
              (write-char #\Space output)
              (if (and (eq 'args name)
                       (typep (issue-form issue)
                              '(cons (member eq eql equal equalp))))
                  (let ((args (slot-value issue name)))
                    (vivid-colors:vprint
                      (list (first args)
                            (vivid-diff:mismatch-sexp (second args)
                                                      (first args)))
                      output t))
                  (vivid-colors:vprint (slot-value issue name) output t))
              (when rest
                (write-char #\Space output)
                (vivid-colors:vprint-newline :linear output)))))

(vivid-colors:define-vprint-dispatch :jingoh
  (:merge :vivid-diff)
  (:set 'issue 'vprint-issue)
  (:set 'unsatisfied-clause 'vprint-unsatisfied-clause))

(defmethod print-object ((issue issue) stream)
  (if (null *print-vivid*)
      (call-next-method)
      (let ((vivid-colors:*vprint-dispatch*
             (vivid-colors:find-vprint-dispatch :jingoh)))
        (vivid-colors:vprint issue stream))))

(defmethod print-object ((issue unsatisfied-clause) stream)
  (if (not *print-vivid*)
      (call-next-method)
      (let ((vivid-colors:*vprint-dispatch*
             (vivid-colors:find-vprint-dispatch :jingoh)))
        (vivid-colors:vprint issue stream))))

(defun should-print-vivid-p (issue)
  (and (not
         (typep issue
                '(or condition-issue
                     unexpected-success
                     unexpected-output
                     missing-restarts)))
       (or (typep (issue-expected issue) '(or sequence pathname array))
           (typep (class-of (issue-expected issue)) 'structure-class))))