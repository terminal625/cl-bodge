(cl:defpackage :cl-bodge.physics.3d.demo
  (:use :cl :cl-bodge.demo.api))
(cl:in-package :cl-bodge.physics.3d.demo)


(defclass 3d-physics-showcase ()
  (pipeline position-buffer index-buffer))
(register-showcase '3d-physics-showcase)


(ge.gx:defsstruct demo-value
  value)


(ge:defshader (demo-shader
               (:sources "demo.glsl")
               (:base-path :system-relative :cl-bodge/demo "cases/3d-physics"))
  (position :name "vPosition")
  (value :name "value"))


(defparameter *value* (make-demo-value :value 0f0))


(ge:defpipeline (demo-pipeline
                 (:primitive :triangle-strip))
  :vertex demo-shader
  :fragment demo-shader)


(defmethod showcase-name ((this 3d-physics-showcase))
  "3D Physics")


(defmethod showcase-revealing-flow ((this 3d-physics-showcase) ui)
  (with-slots (pipeline position-buffer index-buffer) this
    (ge:for-graphics ()
      (multiple-value-bind (vertices normals indices)
          (cl-bodge.demo::generate-sphere-arrays 1 100 100)
        (declare (ignore normals))
      (setf pipeline (ge:make-pipeline 'demo-pipeline)
            position-buffer (ge:make-array-buffer vertices :element-size 3)
            index-buffer (ge:make-index-buffer indices))))))


(defmethod showcase-closing-flow ((this 3d-physics-showcase))
  (with-slots (pipeline position-buffer index-buffer) this
    (ge:dispose index-buffer)
    (ge:dispose position-buffer)
    (ge:dispose pipeline)))


(defmethod render-showcase ((this 3d-physics-showcase))
  (with-slots (pipeline position-buffer index-buffer) this
    (setf (value-of *value*) (float (ge.util:real-time-seconds) 0f0))
    (ge:render t pipeline
               :index-buffer index-buffer
               'position position-buffer
               'value *value*)))
