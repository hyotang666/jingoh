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

;;;; Wrapper of c2mop:SLOT-DEFINITION-NAME for structure class.

#+abcl
(let ((methods (c2mop:generic-function-methods #'c2mop:slot-definition-name))
      (slot (car (c2mop:class-slots (class-of (make-issue))))))
  (flet ((disable-coloring-feature (error-memo)
           (warn "Issue coloring feature is disabled. ~S" error-memo)
           (setq *print-vivid* nil)))
    (cond ;; The guard for implementation of the structure slot definitions.
          ((not (vectorp slot))
           ;; then ABCL changed its implementation.
           ;; Ok, by the way, does it work for us?
           (if (ignore-errors (eq 'form (c2mop:slot-definition-name slot)))
               ;; This workaround code is no longer needed.
               (eval-when (:load-toplevel)
                 (warn "Please cleanup ~S." *load-truename*))
               ;; We must work around.
               (disable-coloring-feature :new-impl)))
          ;; ABCL uses VECTOR to implement structure slot definitions.
          ;; Does nobody define yet?
          ((not
             (find 'vector methods
                   :test (lambda (name specializers)
                           (find name specializers :key #'class-name))
                   :key #'c2mop:method-specializers))
           ;; Ok.
           (defmethod c2mop:slot-definition-name ((a vector))
             (if (eq 'system::defstruct-slot-description (aref a 0))
                 (aref a 1)
                 (call-next-method))))
          ;; Somebody (including us while reloading) defined it already.
          ;; Does it work enough for us?
          ((eq 'form (c2mop:slot-definition-name slot))
           ;; Ok, do nothing.
           nil)
          ;; Otherwise
          ;; 1. Somebody defined it for other purpose.
          ;; 2. ABCL changed the order of elements.
          (t (disable-coloring-feature :order)))))

(defun vprint-issue (output issue)
  (vivid-colors:vprint-logical-block (output nil :prefix "#S(" :suffix ")")
    (vivid-colors:put (type-of issue) output :color cl-colors2:+red+) ; op
    (write-char #\Space output)
    (vivid-colors:vprint-newline :linear output)
    (loop :for (slot . rest) :on (c2mop:class-slots (class-of issue))
          :for name = (c2mop:slot-definition-name slot)
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
          :for name := (c2mop:slot-definition-name slot)
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
  (if (or (not (uiop:featurep :vivid-colors)) (not *print-vivid*))
      (call-next-method)
      (let ((vivid-colors:*vprint-dispatch*
             (vivid-colors:find-vprint-dispatch :jingoh)))
        (vivid-colors:vprint issue stream))))

(defmethod print-object ((issue unsatisfied-clause) stream)
  (if (or (not (uiop:featurep :vivid-colors)) (not *print-vivid*))
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