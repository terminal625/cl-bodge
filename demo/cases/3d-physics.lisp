(cl:defpackage :cl-bodge.physics.3d.demo
  (:use :cl :cl-bodge.demo.api :cl-bodge.demo.scene))
(cl:in-package :cl-bodge.physics.3d.demo)


(defclass 3d-physics-showcase ()
  (scene sphere bulb box))
(register-showcase '3d-physics-showcase)



(defmethod showcase-name ((this 3d-physics-showcase))
  "3D Physics")


(defmethod showcase-revealing-flow ((this 3d-physics-showcase) ui)
  (with-slots (scene sphere bulb box) this
    (ge:for-graphics ()
      (setf scene (make-simple-scene)
            sphere (add-sphere scene)
            bulb (add-sphere scene :radius 0.1)
            box (add-box scene))
      (update-shape sphere :color (ge:vec3 0.2 0.6 0.2))
      (update-shape box :color (ge:vec3 0.2 0.2 0.6))
      (update-shape bulb :color (ge:vec3 1 1 1) :emission-color (ge:vec3 0.8 0.8 0.8)))))


(defmethod showcase-closing-flow ((this 3d-physics-showcase))
  (with-slots (scene) this
    (ge:dispose scene)))


(defmethod render-showcase ((this 3d-physics-showcase))
  (with-slots (scene sphere bulb box) this
    (gl:clear-color 0.1 0.1 0.1 1.0)
    (gl:clear :color-buffer)
    (let ((time (float (ge.util:real-time-seconds) 0f0)))
      (let ((position (ge:vec3 (* 1.2 (sin time))
                               (* 1.2 (cos time))
                               (* (cos time) (sin time)))))
        (update-light scene
                      :color (ge:vec3 1 1 1)
                      :position position)
        (update-shape bulb
                      :transform (ge:translation-mat4 (ge:x position)
                                                      (ge:y position)
                                                      (ge:z position))))
      (update-shape sphere
                    :transform
                    (ge:translation-mat4 1 0 -2))
      (update-shape box
                    :transform
                    (ge:mult (ge:translation-mat4 -1 0 -3)
                             (ge:euler-angles->mat4 (ge:vec3 (* 2 (sin time))
                                                             (* 2 (cos time))
                                                             (* 2 (sin time) (cos time)))))))
    (render-scene scene)))
