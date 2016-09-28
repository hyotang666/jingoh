(in-package :cl-user)
(defpackage :jingoh.reporter(:use :cl :jingoh.org :jingoh.util :resignal-bind :jingoh.tester)
  (:export
    ;;;; main api
    #:report
    #:detail

    ;;;; for customizing reporter
    *reporter*
    #:default-reporter
    #|The default detailer is PRINT|#

    ))

