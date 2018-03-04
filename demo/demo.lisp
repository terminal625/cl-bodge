(cl:defpackage :cl-bodge.demo
  (:use :cl)
  (:export #:run))
(cl:in-package :cl-bodge.demo)


(defclass demo (ge:enableable ge:generic-system)
  ()
  (:default-initargs :depends-on '(ge:host-system ge:graphics-system ge:audio-system)))


(defun render (demo)
  (declare (ignore demo))
  (gl:clear-color 1.0 1.0 1.0 1.0)
  (gl:clear :color-buffer)
  (ge:swap-buffers))


(defmethod ge:initialize-system :after ((this demo))
  (ge:run (flow:serially
           (ge:for-host ()
             (setf (ge:swap-interval) 1))
           (ge:loop-flow
            (flow:serially
             (ge:for-graphics ()
               (render this)))
            (lambda () (ge:enabledp this))))))


(ge:define-event-handler exit-handler ((eve ge:viewport-hiding-event))
  (ge:shutdown))


(defun run ()
  (ge:startup `(:engine (:systems (demo)))))
