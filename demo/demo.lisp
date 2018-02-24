(cl:defpackage :cl-bodge.demo
  (:use :cl)
  (:export #:run))
(cl:in-package :cl-bodge.demo)


(defclass demo (ge:generic-system)
  ()
  (:default-initargs :depends-on '(ge:host-system ge:graphics-system ge:audio-system)))
