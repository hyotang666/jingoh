(in-package :cl-user)
(defpackage :jingoh.tester(:use :cl :jingoh.org :jingoh.util)
  (:export
    ;;;; main api
    ;; notations
    #:defspec
    #:=>
    #:unspecified
    #:implementation-dependent
    #:&
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

    ;;;; Structure
    ;; main
    #:issue #:condition-issue
    ;; miscellaneous
    #:error-was-signaled #:warning-was-signaled #:unexpected-success #:issue-of-multiple-values #:wrong-format #:debugger-was-invoked

    ;;;; predicate
    ;; main
    #:issue-p #:condition-issue-p
    ;; miscellaneous
    #:error-was-signaled-p #:warning-was-signaled-p #:unexpected-success-p #:issue-of-multiple-values-p #:wrong-format-p #:debugger-was-invoked-p

    ;;;; accessor
    #:issue-form #:issue-expected #:issue-actual #:issue-test
    #:condition-issue-message
    #|NOTE! - Slot names are not exported.|#

    #|NOTE! - Constructor is not exported. Use MAKE-INSTANCE|#

    ;;;; condition
    #:syntax-error

    ))
