(in-package :jingoh.org)

(declaim (optimize speed))

(define-condition missing (error)
  ((api :initarg :api :initform nil :reader api :type symbol)
   (datum :initarg :datum :initform nil :reader datum))
  (:report
   (lambda (condition *standard-output*)
     (format t "~@[~S: ~]Searched but ~(~A~)~@[ named ~S~]." (api condition)
             (substitute #\Space #\- (string (type-of condition)))
             (datum condition)))))

(define-condition missing-org (missing) ())

(define-condition missing-subject (missing) ())