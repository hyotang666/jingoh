(defpackage :jingoh.documentizer
  (:use :cl)
  (:shadow #:import #:compile)
  (:export
    #:documentize
    #:github-wiki
    #:import
    ;;;; Variable to control section printer.
    #:*print-example*
    ))
