(cl:defpackage :cl-bodge.physics.3d.demo
  (:use :cl :cl-bodge.demo.api :cl-bodge.demo.scene))
(cl:in-package :cl-bodge.physics.3d.demo)


(defclass 3d-physics-showcase ()
  (scene sphere bulb box ground universe ball-body ball-shape ground-shape ground-body))
(register-showcase '3d-physics-showcase)



(defmethod showcase-name ((this 3d-physics-showcase))
  "3D Physics")


(defmethod showcase-revealing-flow ((this 3d-physics-showcase) ui)
  (with-slots (scene sphere bulb box ground
               universe ball-body ball-shape ground-shape ground-body)
      this
    (ge:>>
     (ge:instantly ()
       (setf universe (ge:make-universe :3d)
             (ge.phy:gravity universe) (ge:vec3 0 -0.01 0)
             ball-body (ge.phy:make-rigid-body universe)
             (ge.phy:body-position ball-body) (ge:vec3 0 0 -2)
             ball-shape (ge.phy:make-sphere-shape universe 1 :body ball-body)
             ground-body (ge.phy:make-kinematic-body universe)
             (ge.phy:body-position ground-body) (ge:vec3 0 -1.5 -2)
             ground-shape (ge.phy:make-cuboid-shape universe 10 0.05 10 :body ground-body)))
     (ge:for-graphics ()
       (setf scene (make-simple-scene)
             sphere (add-sphere scene)
             bulb (add-sphere scene :radius 0.1)
             box (add-box scene)
             ground (add-box scene :x 10 :y 0.05 :z 10))
       (update-shape sphere :color (ge:vec3 0.2 0.6 0.2))
       (update-shape bulb :color (ge:vec3 1 1 1) :emission-color (ge:vec3 0.8 0.8 0.8))
       (update-shape box :color (ge:vec3 0.2 0.2 0.6))
       (update-shape ground :color (ge:vec3 0.6 0.3 0.4))))))



(defmethod showcase-closing-flow ((this 3d-physics-showcase))
  (with-slots (scene universe ball-body ball-shape) this
    (ge:dispose ball-body)
    (ge:dispose ball-shape)
    (ge:dispose universe)
    (ge:dispose scene)))


(defmethod render-showcase ((this 3d-physics-showcase))
  (with-slots (scene sphere bulb box universe ball-body ground ground-body) this
    (ge:observe-universe universe 0.14)
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
                    (ge:vec->translation-mat4 (ge:body-position ball-body)))

      (update-shape sphere
                    :transform (ge:vec->translation-mat4 (ge:body-position ball-body)))
      (update-shape ground
                    :transform (ge:vec->translation-mat4 (ge:body-position ground-body)))
      (update-shape box
                    :transform
                    (ge:mult (ge:translation-mat4 -1 0 -3)
                             (ge:euler-angles->mat4 (ge:vec3 (* 2 (sin time))
                                                             (* 2 (cos time))
                                                             (* 2 (sin time) (cos time)))))))
    (render-scene scene)))
