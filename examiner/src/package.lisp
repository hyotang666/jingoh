(in-package :cl-user)
(defpackage :jingoh.examiner(:use :cl :jingoh.org :resignal-bind :jingoh.tester)
  (:export
    ;;;; main api
    #:examine

    ;;;; variables
    #:*verbose*
    #:*stop-on-fails*
    #:*break-on-fails*
    #:*issues*
    ))

