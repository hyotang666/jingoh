(defpackage :jingoh.documentizer
  (:use :cl :jingoh.documentizer.utility :jingoh.documentizer.dsl
	:jingoh.documentizer.parse-spec)
  (:export
    #:documentize
    #:github-wiki
    ))
