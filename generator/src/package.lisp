(defpackage :jingoh.generator(:use :cl)
  (:export
    #:generate
    #:add-method-extension
    ))
(in-package :jingoh.generator)

(defgeneric generate(arg &key))

(deftype system-designator()
  '(or keyword string asdf:system))

(declaim (ftype (function (system-designator)
			  (values null &optional))
		add-method-extension))
