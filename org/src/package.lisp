(in-package :cl-user)

(defpackage :jingoh.org
  (:use :cl :resignal-bind)
  (:nicknames :org)
  (:export #| main api for light users |#
           #:deforg
           #:in-org
           #:requirements-about
           #:common-requirements-about
           #| api for hackers |#
           ;;;; structure
           #:org ; type
           #:org-p ; predicate
           #:make-org ; constructor
           #:org-name ; reader
           #:org-package ; reader
           #:org-current-subjects ; accessor
           #:org-options ; accessor
           #:org-specifications ; reader
           #|NOTE! - Slot names are not exported.|#
           ;;;; abstract layer to extend org option keys safely.
           #:add-new-option-key
           #:find-option-key
           #:delete-option-key
           #:list-all-option-keys
           #:clear-option-keys
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
           #:*org* ; current org
           ;;;; conditions
           #:not-org
           #:missing
           #:missing-org
           #:missing-subject
           ;; readers
           #:api
           #:datum))