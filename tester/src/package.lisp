(in-package :cl-user)
(defpackage :jingoh.tester(:use :cl :jingoh.org)
  (:import-from :documentation-embedder #:Doc)
  (:export
    ;;;; main api
    ;; notations
    #:defspec
    #:=>
    #:unspecified
    #:implementation-dependent
    #:&
    #:call-body
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
    #:option-form

    ;;;; Structure
    ;; main
    #:issue #:condition-issue
    ;; miscellaneous
    #:error-was-signaled #:warning-was-signaled #:unexpected-success #:issue-of-multiple-values #:wrong-format #:debugger-was-invoked #:missing-restarts

    ;;;; predicate
    ;; main
    #:issue-p #:condition-issue-p
    ;; miscellaneous
    #:error-was-signaled-p #:warning-was-signaled-p #:unexpected-success-p #:issue-of-multiple-values-p #:wrong-format-p #:debugger-was-invoked-p #:missing-restarts-p

    ;;;; accessor
    #:issue-form #:issue-expected #:issue-actual #:issue-test #:issue-position
    #:condition-issue-message
    #|NOTE! - Slot names are not exported.|#

    #|NOTE! - Constructor is not exported. Use MAKE-INSTANCE|#

    ;;;; condition
    #:syntax-error

    ))