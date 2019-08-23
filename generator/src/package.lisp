(defpackage :jingoh.generator(:use :cl)
  (:export
    #:generate
    #:add-method-extension
    ))
(in-package :jingoh.generator)

(defgeneric generate(arg &key))
(declaim (ftype (function (asdf:system)
			  (values null &optional))
		add-method-extension))
