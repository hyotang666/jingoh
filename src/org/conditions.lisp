(in-package :jingoh.org)

(define-condition not-org(type-error)
  ((api :initarg :api :reader api :type symbol))
  (:report(lambda(condition *standard-output*)
	    (format t "~S: ~S is not type of ~S"
		    (api condition)
		    (type-error-datum condition)
		    (type-error-expected-type condition))))
  (:documentation #.(doc :jingoh.org "doc/org/not-org.T.md")))

(define-condition missing(error)
  ((api :initarg :api :initform nil :reader api :type symbol)
   (datum :initarg :datam :initform nil :reader datum))
  (:report(lambda(condition *standard-output*)
	    (format t "~@[~S: ~]Searched but ~(~A~)~@[ named ~S~]."
		    (api condition)
		    (substitute #\space #\-(string(type-of condition)))
		    (datum condition))))
  (:documentation #.(doc :jingoh.org "doc/org/missing.T.md")))

(define-condition missing-org(missing)()
  (:documentation #.(doc :jingoh.org "doc/org/missing-org.T.md")))

(define-condition missing-subject(missing)()
  (:documentation #.(doc :jingoh.org "doc/org/missing-subject.T.md")))
