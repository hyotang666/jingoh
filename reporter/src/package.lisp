(in-package :cl-user)
(defpackage :jingoh.reporter(:use :cl :jingoh.org :resignal-bind :jingoh.tester)
  (:import-from :documentation-embedder #:Doc)
  (:export
    ;;;; main api
    #:report
    #:detail
    #:verify

    ;;;; reporter and detailer. 
    *reporter*
    #:default-reporter
    #|The default detailer is PRINT.
      You can costomize it with
      (defmethod print-object((obj jingoh.tester:issue)stream)
	 ...)
      .|#

    ))

