(in-package :cl-user)

(defpackage :jingoh.org
  (:use :cl)
  (:nicknames :org)
  (:export ;;;; Main api for light users.
           #:deforg
           #:in-org
           #:requirements-about
           #:common-requirements-about)
  (:export ;;;; API for hackers.
           ;;;; structure
           ;; NOTE! - Slot names are not exported.
           #:org ; type
           #:org-p ; predicate
           #:make-org ; constructor
           #:org-name ; reader
           #:org-package ; reader
           #:org-current-subjects ; accessor
           #:org-options ; accessor
           #:org-specifications ; reader
           )
  (:export ;;;; Abstract layers to extend org option keys safely.
           #:add-new-option-key
           #:find-option-key
           #:delete-option-key
           #:list-all-option-keys
           #:clear-option-keys)
  (:export ;;;; Miscellaneous
           ;; org operators
           #:register-org
           #:find-org
           #:delete-org
           #:org-requirements-count
           ;; requirements operators
           #:do-requirements
           #:add-requirement
           #:delete-subject
           ;; type specifier
           #:org-designator
           #:subject-designator
           ;;;; variable
           #:*org* ; current org
           )
  (:export ;;;; conditions
           #:missing
           #:missing-org
           #:missing-subject
           ;; readers
           #:api
           #:datum))
