(cl:defpackage :cl-bodge.physics.3d.demo
  (:use :cl :cl-bodge.demo.api))
(cl:in-package :cl-bodge.physics.3d.demo)


(defclass 3d-physics-showcase ()
  (pipeline position-buffer index-buffer normal-buffer))
(register-showcase '3d-physics-showcase)


(ge.gx:defsstruct demo-value
  value)


(defparameter *value* (make-demo-value :value 0f0))
(defparameter *view-mat* (ge:translation-mat4 0.0 0.0 -6.0))
(defparameter *mvp* (ge:mult (ge:perspective-projection-mat 1 (/ 600 800) 1.0 100.0)
                             *view-mat*))
(defparameter *light* (ge.shad::make-phong-point-light :position (ge:vec3 0 0 0)
                                                       :color (ge:vec3 1 1 1)
                                                       :ambient (ge:vec3 0.1 0.1 0.1)
                                                       :falloff 0.15f0
                                                       :radius 100f0))
(defparameter *material* (ge.shad::make-phong-material :specular-scale 0.65f0
                                                       :shininess 25f0
                                                       :roughness 10f0
                                                       :albedo 0.95f0))
(defparameter *color* (ge:vec3 1.0 1.0 1.0))

(ge:defshader (demo-shader
               (:sources "demo.glsl")
               (:base-path :system-relative :cl-bodge/demo "cases/3d-physics"))
  (position :name "vPosition")
  (normal :name "vNormal")
  (mvp :name "MVP")
  (view :name "V")
  (material :name "material")
  (light :name "light")
  (color :name "diffuseColor")
  (value :name "value"))


(ge:defpipeline (demo-pipeline
                 (:primitive :triangle-strip))
  :vertex demo-shader
  :fragment demo-shader)


(defmethod showcase-name ((this 3d-physics-showcase))
  "3D Physics")


(defmethod showcase-revealing-flow ((this 3d-physics-showcase) ui)
  (with-slots (pipeline position-buffer index-buffer normal-buffer) this
    (ge:for-graphics ()
      (multiple-value-bind (vertices normals indices)
          (cl-bodge.demo::generate-sphere-arrays 1 100 100)

      (setf pipeline (ge:make-pipeline 'demo-pipeline)
            position-buffer (ge:make-array-buffer vertices :element-size 3)
            normal-buffer (ge:make-array-buffer normals :element-size 3)
            index-buffer (ge:make-index-buffer indices))))))


(defmethod showcase-closing-flow ((this 3d-physics-showcase))
  (with-slots (pipeline position-buffer index-buffer normal-buffer) this
    (ge:dispose normal-buffer)
    (ge:dispose index-buffer)
    (ge:dispose position-buffer)
    (ge:dispose pipeline)))


(defmethod render-showcase ((this 3d-physics-showcase))
  (with-slots (pipeline position-buffer index-buffer normal-buffer) this
    (let ((time (float (ge.util:real-time-seconds) 0f0)))
      (setf (demo-value-value *value*) time
            (ge.shad::phong-point-light-color *light*) (ge:vec3 (cos time) 1.0 (sin time))
            (ge.shad::phong-point-light-position *light*) (ge:vec3 (cos time) (sin time) 0)))
    (ge:render t pipeline
               :index-buffer index-buffer
               'position position-buffer
               'normal normal-buffer
               'value *value*
               'material *material*
               'light *light*
               'mvp *mvp*
               'color *color*
               'view *view-mat*)))
