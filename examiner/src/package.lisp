(in-package :cl-user)
(defpackage :jingoh.examiner(:use :cl :jingoh.org :resignal-bind :jingoh.tester)
  (:import-from :documentation-embedder #:Doc)
  (:export
    ;;;; main api
    #:examine

    ;;;; variables
    #:*verbose*
    #:*stop-on-fails*
    #:*break-on-fails*
    #:*issues*

    ;;;; internal api
    #:mismatch-sexp
    ))

