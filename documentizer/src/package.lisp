(defpackage :jingoh.documentizer
  (:use :cl)
  (:shadow #:import)
  (:export
    #:documentize
    #:github-wiki
    #:lisp
    #:import
    #:importer
    ;;;; Variable to control section printer.
    #:*print-example*
    ))
