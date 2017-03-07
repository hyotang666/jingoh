(in-package :cl-user)
(defpackage :jingoh.tester(:use :cl :jingoh.org)
  (:export
    ;;;; main api
    ;; notations
    #:defspec
    #:=>
    #:unspecified
    #:implementation-dependent
    #:call-body
    #:&
    #:sexp=
    ;; evaluator
    #:?
    #:check

    ;;;; for internal dsl
    #:make-requirement
    #:the-standard-handling-form
    #:the-push-instance-form
    #:reserved-keywords
    #:encallable
    #:canonicalize
    #:with-integrated-output-stream

    ;;;; Structure
    ;; main
    #:issue #:condition-issue #:test-issue
    ;; miscellaneous
    #:error-was-signaled #:warning-was-signaled #:unexpected-success #:issue-of-multiple-values #:wrong-format #:debugger-was-invoked #:missing-restarts #:unexpected-output #:unmatch-condition #:unsatisfied-clause

    ;;;; predicate
    ;; main
    #:issue-p #:condition-issue-p #:test-issue-p
    ;; miscellaneous
    #:error-was-signaled-p #:warning-was-signaled-p #:unexpected-success-p #:issue-of-multiple-values-p #:wrong-format-p #:debugger-was-invoked-p #:missing-restarts-p #:unexpected-output-p #:unmatch-condition-p #:unsatisfied-clause-p

    ;;;; accessor
    #:issue-form #:issue-expected #:issue-actual #:issue-position
    #:test-issue-test
    #:condition-issue-message
    #:unsatisfied-clause-args
    #|NOTE! - Slot names are not exported.|#

    #|NOTE! - Constructor is not exported. Use MAKE-INSTANCE|#

    ;;;; printer control
    #:*print-vivid*

    ;;;; condition
    #:syntax-error

    ))

(in-package :jingoh.tester)

(defvar *substituter*)
