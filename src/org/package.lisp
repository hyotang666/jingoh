(in-package :cl-user)
(defpackage :jingoh.org(:use :cl :jingoh.util :with-resignal)
  (:nicknames :org)
  (:export

    #| main api for light users |#
    #:deforg
    #:in-org
    #:requirements-about

    #| api for hackers |#
    ;;;; structure
    #:org ; type
    #:org-p ; predicate
    #:make-org ; constructor
    #:org-name ; reader
    #:org-current-subject ; accessor
    #:org-specifications ; reader
    #|NOTE! - Slot names are not exported.|#

    ;;;; miscellaneous
    ;; org operators
    #:register-org
    #:find-org
    #:delete-org
    #:org-requirements-count
    ;; requirements operators
    #:map-requirements
    #:do-requirements
    #:add-requirement
    #:delete-subject
    ;; type specifier
    #:org-designator
    #:subject-designator

    ;;;; variable
    *org* ; current org
    *subject* ; current subject (psued variable)
    *options* ; current subject's options (psued variable)

    ;;;; conditions
    #:not-org
    #:missing
    #:missing-org
    #:missing-subject
    ;; readers
    #:api
    #:datum

    ))

