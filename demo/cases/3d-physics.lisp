(cl:defpackage :cl-bodge.physics.3d.demo
  (:use :cl :cl-bodge.demo.api :cl-bodge.demo.scene))
(cl:in-package :cl-bodge.physics.3d.demo)


(defclass 3d-physics-showcase ()
  (scene universe bulb
   (objects :initform nil)))
(register-showcase '3d-physics-showcase)



(defmethod showcase-name ((this 3d-physics-showcase))
  "3D Physics")


(defclass demo-object (ge:disposable)
  ((drawable :initarg :drawable)
   (shape :initarg :shape)))


(defun make-demo-object (drawable shape)
  (make-instance 'demo-object :drawable drawable :shape shape))


(defun update-object (object)
  (with-slots (drawable shape) object
    (let ((body (ge:shape-body shape)))
      (update-drawable drawable
                       :transform (ge:mult (ge:vec->translation-mat4 (ge:body-position body))
                                           (ge:mat3->mat4 (ge:body-rotation body)))))))


(defun transform-object (object position-vec rotation-vec)
  (with-slots (shape) object
    (let ((body (ge:shape-body shape)))
      (setf (ge:body-position body) position-vec
            (ge:body-rotation body) (ge:euler-angles->mat3 rotation-vec))
      (update-object object))))


(defmethod showcase-revealing-flow ((this 3d-physics-showcase) ui)
  (with-slots (scene bulb universe objects) this
    (flet ((add-object (drawable shape position color)
             (update-drawable drawable :color color)
             (let ((object (make-demo-object drawable shape)))
               (transform-object object position (ge:vec3))
               (push object objects))))
    (ge:>>
     (ge:instantly ()
       (setf universe (ge:make-universe :3d)
             (ge.phy:gravity universe) (ge:vec3 0 -0.1 0)))
     (ge:for-graphics ()
       (setf scene (make-simple-scene))
       (add-object (add-sphere scene)
                   (ge.phy:make-sphere-shape universe 1
                                             :body (ge.phy:make-rigid-body universe))
                   (ge:vec3 0 1 -1)
                   (ge:vec3 0.2 0.6 0.2))
       (add-object (add-box scene)
                   (ge.phy:make-cuboid-shape universe 1 1 1
                                             :body (ge.phy:make-rigid-body universe))
                   (ge:vec3 1 3 -1)
                   (ge:vec3 0.2 0.2 0.6))
       (let ((x 10)
             (y 0.05)
             (z 10))
         (add-object (add-box scene :x x :y y :z z)
                     (ge.phy:make-cuboid-shape universe x y z
                                               :body (ge.phy:make-kinematic-body universe))
                     (ge:vec3 0 -1.5 -2)
                     (ge:vec3 0.6 0.3 0.4)))
       (let* ((radius 0.1)
              (bulb-drawable (add-sphere scene :radius radius)))
         (update-drawable bulb-drawable :color (ge:vec3 1 1 1)
                                        :emission-color (ge:vec3 0.8 0.8 0.8))
         (setf bulb (make-demo-object
                     bulb-drawable
                     (ge.phy:make-sphere-shape universe radius
                                               :body (ge.phy:make-kinematic-body universe))))))))))


(defmethod showcase-closing-flow ((this 3d-physics-showcase))
  (with-slots (scene universe objects) this
    (ge:dispose universe)
    (ge:dispose scene)))


(defmethod render-showcase ((this 3d-physics-showcase))
  (with-slots (scene universe bulb objects) this
    (gl:clear-color 0.1 0.1 0.1 1.0)
    (gl:clear :color-buffer)

    (ge:observe-universe universe 0.14)
    (loop for object in objects
          do (update-object object))

    (let ((time (float (ge.util:real-time-seconds) 0f0)))
      (let ((position (ge:vec3 (* 1.2 (sin time))
                               (* 1.2 (cos time))
                               (* (cos time) (sin time)))))
        (update-light scene :position position)
        (transform-object bulb position (ge:vec3)))
    (render-scene scene))))
