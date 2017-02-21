(in-package :cl-user)
(defpackage :jingoh
  (:use :cl :jingoh.org :jingoh.reporter :jingoh.tester :jingoh.reader
	:named-readtables)
  (:export
    ;;;; main api
    #:setup

    ;;;; from org
    #:deforg
    #:in-org
    #:requirements-about

    ;;;; from reader
    #:enable
    #:syntax

    ;;;; from tester
    #:=>
    #:unspecified
    #:implementation-dependent
    #:?
    #:&

    ;;;; Issues for pretty printings.
    #:issue #:condition-issue #:test-issue #:error-was-signaled #:warning-was-signaled #:unexpected-success #:issue-of-multiple-values #:wrong-format #:debugger-was-invoked #:missing-restarts #:unexpected-output #:unmatch-condition #:unsatisfied-clause

    ;;;; from reporter
    #:report
    #:detail
    #:verify
    ))
(in-package :jingoh)

(defmacro setup(name)
  (check-type name symbol)
  `(PROGN
     (DEFORG ,name)
     (IN-ORG ,name)
     (IN-READTABLE SYNTAX)))
