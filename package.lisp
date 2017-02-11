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
