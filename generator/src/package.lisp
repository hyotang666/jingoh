(defpackage :jingoh.generator
  (:use :cl)
  (:shadow #:dribble)
  (:export #:generate
           #:add-method-extension
           #:dribble
           #:define-special-command
           #:*spec-output*))

(in-package :jingoh.generator)

(defgeneric generate (arg &key))

(deftype system-designator () '(or keyword string asdf:system))

(declaim
 (ftype (function (system-designator) (values null &optional))
        add-method-extension))