(cl:defpackage :cl-bodge.pbr.demo
  (:use :cl :cl-bodge.demo.api))
(cl:in-package :cl-bodge.pbr.demo)


(defclass pbr-showcase () ())


(register-showcase 'pbr-showcase)


(defmethod showcase-name ((this pbr-showcase))
  "PBR")


(defmethod showcase-revealing-flow ((this pbr-showcase) ui)
  (ge:>>
   (ge:for-graphics () )))


(defmethod showcase-closing-flow ((this pbr-showcase)))


(defmethod render-showcase ((this pbr-showcase))
  (ge:clear-rendering-output t :color (ge:vec4 0.2 0.2 0.2 1.0)))
