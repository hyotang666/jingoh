(defpackage :jingoh.documentizer
  (:use :cl :jingoh.documentizer.utility :jingoh.documentizer.dsl
	:jingoh.documentizer.sections :jingoh.documentizer.parse-spec)
  (:shadow #:import)
  (:export
    #:documentize
    #:github-wiki
    #:lisp
    #:import
    ))
