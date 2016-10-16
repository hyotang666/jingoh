(in-package :cl-user)
(defpackage :jingoh.reporter(:use :cl :jingoh.org :resignal-bind :jingoh.tester)
  (:import-from :documentation-embedder #:doc)
  (:export
    ;;;; main api
    #:report
    #:detail

    ;;;; reporter and detailer. 
    *reporter*
    #:default-reporter
    #|The default detailer is PRINT.
      You can costomize it with
      (defmethod print-object((obj jingoh.tester:issue)stream)
	 ...)
      .|#

    ))

