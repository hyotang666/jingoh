(in-package :cl-user)
(defpackage :jingoh
  (:use :cl :jingoh.org :jingoh.examiner :jingoh.tester :jingoh.reader
	:named-readtables)
  (:export
    ;;;; main api
    #:setup

    ;;;; from org
    #:deforg
    #:in-org
    #:requirements-about
    #:common-requirements-about

    ;;;; from reader
    #:enable
    #:syntax

    ;;;; from tester
    #:=>
    #:unspecified
    #:implementation-dependent
    #:?
    #:&
    #:call-body

    ;; Issue names for pretty printings.
    #:issue #:condition-issue #:test-issue #:error-was-signaled #:warning-was-signaled #:unexpected-success #:issue-of-multiple-values #:wrong-format #:debugger-was-invoked #:missing-restarts #:unexpected-output #:unmatch-condition #:unsatisfied-clause

    ;; slot names for pretty printings, especially for ECL.
    #:form
    #:expected
    #:actual
    #:test
    #:message
    #:args

    ;; config
    #:*print-vivid*

    ;;;; from examiner
    #:examine

    ;; config
    #:*break-on-fails*
    #:*break-on-finish*
    #:*stop-on-fails*
    #:*verbose*

    ))
(in-package :jingoh)

(defmacro setup(name)
  (check-type name symbol)
  `(PROGN
     (DEFORG ,name)
     (IN-ORG ,name)
     (IN-READTABLE SYNTAX)))
