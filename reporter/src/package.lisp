(in-package :cl-user)
(defpackage :jingoh.reporter(:use :cl :jingoh.org :resignal-bind :jingoh.tester)
  (:import-from :documentation-embedder #:doc)
  (:export
    ;;;; main api
    #:report
    #:detail

    ;;;; for customizing reporter
    *reporter*
    #:default-reporter
    #|The default detailer is PRINT|#

    ))

