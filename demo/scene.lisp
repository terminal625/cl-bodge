(cl:defpackage :cl-bodge.demo.scene
  (:use :cl)
  (:export #:make-simple-scene
           #:render-scene
           #:update-light
           #:update-shape
           #:add-sphere
           #:add-box))
(cl:in-package :cl-bodge.demo.scene)


(ge:defshader (demo-shader
               (:sources "demo.glsl")
               (:base-path :system-relative :cl-bodge/demo))
  (position :name "vPosition")
  (normal :name "vNormal")
  (color :name "diffuseColor")
  (emission-color :name "emissionColor")
  (model :name "model")
  (view :name "view")
  (projection :name "projection")
  (material :name "material")
  (light :name "light"))


(ge:defpipeline (demo-pipeline
                 (:primitive :triangle-strip))
  :vertex demo-shader
  :fragment demo-shader)


(defclass scene (ge:disposable)
  ((pipeline :reader %pipeline-of)
   (light :initform (ge.shad::make-phong-point-light :position (ge:vec3 0 0 0)
                                                     :color (ge:vec3 1 1 1)
                                                     :ambient (ge:vec3 0.3 0.3 0.3)
                                                     :falloff 0.15f0
                                                     :radius 100f0)
          :reader %light-of)
   (proj :initform (ge:perspective-projection-mat 1 (/ 480 640) 1.0 100.0)
         :accessor %proj-of)
   (view :initform (ge:translation-mat4 0.0 0.0 -6.0)
         :accessor %view-of)
   (shapes :initform (list nil))))


(ge:define-destructor scene (pipeline shapes)
  (loop for shape in shapes
        when shape do (ge:dispose shape))
  (ge:dispose pipeline))


(defmethod initialize-instance :after ((this scene) &key)
  (with-slots (pipeline) this
    (setf pipeline (ge:make-pipeline 'demo-pipeline))))


(defun update-light (scene &key color position)
  (with-slots (light) scene
    (when color
      (setf (ge:phong-point-light-color light) color))
    (when position
      (setf (ge:phong-point-light-position light) position))))


(defun make-simple-scene ()
  (make-instance 'scene))


(defclass shape (ge:disposable)
  ((position-buffer)
   (normal-buffer)
   (index-buffer)
   (transform :initform (ge:identity-mat4))
   (color :initform (ge:vec3 1 1 1))
   (emission-color :initform (ge:vec3 0 0 0))
   (material :initform (ge.shad::make-phong-material :specular-scale 0.65f0
                                                     :shininess 25f0
                                                     :roughness 10f0
                                                     :albedo 0.95f0))))


(defmethod initialize-instance :after ((this shape) &key vertex-generator)
  (with-slots (position-buffer normal-buffer index-buffer) this
    (multiple-value-bind (vertices normals indices)
        (funcall vertex-generator)
      (setf position-buffer (ge:make-array-buffer vertices :element-size 3)
            normal-buffer (ge:make-array-buffer normals :element-size 3)
            index-buffer (ge:make-index-buffer indices)))))


(ge:define-destructor shape (position-buffer normal-buffer index-buffer)
  (ge:dispose position-buffer)
  (ge:dispose normal-buffer)
  (ge:dispose index-buffer))


(defun render-shape (scene shape)
  (with-slots (position-buffer normal-buffer index-buffer material
               transform color emission-color)
      shape
    (ge:render t (%pipeline-of scene)
               :index-buffer index-buffer
               'position position-buffer
               'normal normal-buffer
               'material material
               'light (%light-of scene)
               'model transform
               'view (%view-of scene)
               'projection (%proj-of scene)
               'color color
               'emission-color emission-color)))



(defun update-shape (shape &key color transform material emission-color)
  (with-slots ((this-color color)
               (this-transform transform)
               (this-material material)
               (this-emission-color emission-color))
      shape
    (when color
      (setf this-color color))
    (when transform
      (setf this-transform transform))
    (when material
      (setf this-material material))
    (when emission-color
      (setf this-emission-color emission-color))))


(defclass sphere (shape) ())


(defun add-sphere (scene &key color position rotation (radius 1.0))
  (declare (ignore color position rotation))
  (with-slots (shapes) scene
    (flet ((vertex-gen ()
             (cl-bodge.demo::generate-sphere-arrays radius 100 100)))
      (let ((sphere (make-instance 'sphere :vertex-generator #'vertex-gen)))
      (push sphere shapes)
      sphere))))


(defclass box (shape) ())


(defun render-scene (scene)
  (with-slots (pipeline shapes) scene
    (loop for shape in shapes
          when shape
            do (render-shape scene shape))))
