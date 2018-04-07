(cl:defpackage :cl-bodge.physics.demo
  (:use :cl :cl-bodge.demo.api))
(cl:in-package :cl-bodge.physics.demo)


(defparameter *unit-scale* 0.1)
(defparameter *ball-radius* 5)
(defparameter *ball-position* (ge:vec2 0 15))
(defparameter *ground-position* (list (ge:vec2 -10 5) (ge:vec2 20 -5)))


(defclass 2d-physics-showcase ()
  (universe ground ball-body ball-shape canvas))


(register-showcase '2d-physics-showcase)


(defmethod showcase-name ((this 2d-physics-showcase))
  "2D Physics")


(ge:defwindow 2d-physics-panel)


(defmethod showcase-revealing-flow ((this 2d-physics-showcase) ui)
  (with-slots (universe ground ball-body ball-shape canvas) this
    (ge:>>
     (ge:instantly ()
      (setf universe (ge:make-universe :2d)
            ground (ge:make-segment-shape universe
                                          (first *ground-position*)
                                          (second *ground-position*))
            ball-body (ge:make-rigid-body universe)
            ball-shape (ge:make-circle-shape universe 5 :body ball-body))
        (ge:infuse-circle-mass ball-body 5 1)
        (setf (ge:gravity universe) (ge:vec2 0 -9.81)
              (ge:body-position ball-body) *ball-position*))
     (ge:for-host ()
       (ge:viewport-size))
     (ge:for-graphics (viewport-size)
       (setf canvas (ge:make-canvas (ge:x viewport-size) (ge:y viewport-size)))))))


(defmethod showcase-closing-flow ((this 2d-physics-showcase))
  (with-slots (universe ground ball-body ball-shape) this
    (ge:instantly ()
      (ge:dispose ball-shape)
      (ge:dispose ground)
      (ge:dispose ball-body)
      (ge:dispose universe))))


(defmethod render-showcase ((this 2d-physics-showcase))
  (with-slots (universe ground ball-body ball-shape canvas) this
    (ge:with-canvas (canvas)
      (ge:with-pushed-canvas ()
        (ge:translate-canvas 300 200)
        (ge:draw-circle (ge:div (ge:body-position ball-body) *unit-scale*)
                        (/ *ball-radius* *unit-scale*)
                        :fill-paint (ge:vec4 0 0 0 1))
        (ge:draw-line (ge:div (first *ground-position*) *unit-scale*)
                      (ge:div (second *ground-position*) *unit-scale*)
                      (ge:vec4 0 0 0 1))))
    (ge:observe-universe universe 0.014)))
