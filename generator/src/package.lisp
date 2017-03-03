(defpackage :jingoh.generator(:use :cl)
  (:export
    #:generate
    ))
(in-package :jingoh.generator)

(defgeneric generate(arg &key))

