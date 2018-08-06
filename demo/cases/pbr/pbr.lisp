(cl:defpackage :cl-bodge.pbr.demo
  (:use :cl :cl-bodge.demo.api :cl-bodge.demo.scene))
(cl:in-package :cl-bodge.pbr.demo)


(ge:defshader (pbr-vert
               (:sources "pbr-vert.glsl")
               (:base-path (merge-showcase-pathname "pbr/")))
  (position :name "a_Position" :size 3)
  (normal :name "a_Normal" :size 3)
  (tangent :name "a_Tangent" :size 3)
  (uv :name "a_UV" :size 2)
  (mvp :name "u_MVPMatrix")
  (model-mat :name "u_ModelMatrix")
  (normal-mat :name "u_NormalMatrix"))


(ge:defshader (pbr-frag
               (:sources "pbr-frag.glsl")
               (:base-path (merge-showcase-pathname "pbr/")))
  (light-direction :name "u_LightDirection")
  (light-color :name "u_LightColor")
  (diffuse-env-sampler :name "u_DiffuseEnvSampler")
  (specular-env-sampler :name "u_SpecularEnvSampler")
  (brdf-lut :name "u_brdfLUT")
  (base-color-sampler :name "u_BaseColorSampler")
  (normal-sampler :name "u_NormalSampler")
  (normal-scale :name "u_NormalScale")
  (emissive-sampler :name "u_EmissiveSampler")
  (emissive-factor :name "u_EmissiveFactor")
  (metallic-roughness-sampler :name "u_MetallicRoughnessSampler")
  (occlusion-sampler :name "u_OcclusionSampler")
  (occlusion-strength :name "u_OcclusionStrength")
  (metallic-roughness-values :name "u_MetallicRoughnessValues")
  (base-color-factor :name "u_BaseColorFactor")
  (camera :name "u_Camera")
  (scale-diff-base :name "u_ScaleDiffBaseMR")
  (scale-fgd-spec :name "u_ScaleFGDSpec")
  (scale-ibl-ambient :name "u_ScaleIBLAmbient"))


(defparameter *scale-diff-base* (ge:vec4 1.0 1.0 1.0 1.0))
(defparameter *scale-fgd-spec* (ge:vec4 1.0 1.0 1.0 1.0))
(defparameter *scale-ibl-ambient* (ge:vec4 1.0 1.0 1.0 1.0))


(ge:defpipeline pbr-pipeline
  :vertex pbr-vert
  :fragment pbr-frag)


(defvar *scene* nil)
(defvar *pbr-pipeline* nil)

(defparameter *projection-matrix* (ge:perspective-projection-mat 1 (/ 600 800) 1 10))


(defclass pbr-mesh (ge:disposable)
  ((primitive :reader primitive-of)
   (position-array :reader position-array-of)
   (index-array :reader index-array-of)
   (normal-array :reader normal-array-of)
   (tangent-array :reader tangent-array-of)
   (tex-coord-array :reader tex-coord-array-of)))


(ge:define-destructor pbr-mesh (position-array index-array normal-array tangent-array tex-coord-array)
  (ge:dispose position-array)
  (ge:dispose index-array)
  (ge:dispose normal-array)
  (ge:dispose tangent-array)
  (ge:dispose tex-coord-array))


(defmethod initialize-instance :after ((this pbr-mesh) &key resource)
  (with-slots (position-array index-array normal-array tangent-array tex-coord-array primitive)
      this
    (setf primitive (ge:mesh-resource-primitive resource)
          position-array (ge:make-array-buffer (ge:mesh-resource-position-array resource)
                                               :element-size 3)
          index-array (ge:make-index-buffer (ge:mesh-resource-index-array resource))
          normal-array (ge:make-array-buffer (ge:mesh-resource-normal-array resource)
                                             :element-size 3)
          tangent-array (ge:make-array-buffer (ge:mesh-resource-tangent-array resource)
                                              :element-size 3)
          tex-coord-array (ge:make-array-buffer (ge:mesh-resource-tex-coord-array resource 0)
                                                :element-size 2))))


(defclass pbr-material (ge:disposable) ())


(defmethod initialize-instance :after ((this pbr-material) &key resource)
  (with-slots () this))


(defclass pbr-scene (ge:disposable)
  ((mesh-table :initform (make-hash-table))
   (material-table :initform (make-hash-table))
   (texture-table :initform (make-hash-table :test #'equal))))


(ge:define-destructor pbr-scene (mesh-table material-table texture-table)
  (loop for mesh being the hash-value of mesh-table
        do (ge:dispose mesh))
  (loop for material being the hash-value of material-table
        do (ge:dispose material))
  (loop for texture being the hash-value of texture-table
        do (ge:dispose texture)))


(defmethod initialize-instance :after ((this pbr-scene) &key resource base-path)
  (with-slots (mesh-table material-table texture-table) this
    (ge:do-scene-resource-meshes (mesh id resource)
      (setf (gethash id mesh-table) (make-instance 'pbr-mesh :resource mesh)))
    (ge:do-scene-resource-materials (material id resource)
      (setf (gethash id material-table) (make-instance 'pbr-material :resource material))
      (ge:do-material-resource-textures (texture type id material)
        (let ((texture-name (namestring (uiop:enough-pathname (ge:texture-resource-name texture) "/"))))
          (unless (gethash texture-name texture-table)
            (let ((image (ge:load-resource (fad:merge-pathnames-as-file base-path texture-name))))
              (setf (gethash texture-name texture-table) (ge:make-2d-texture image :rgba)))))))))


(defun scene-texture (scene name)
  (with-slots (texture-table) scene
    (alexandria:if-let ((tex (gethash name texture-table)))
      tex
      (error "Texture '~A' not found" name))))


(defun for-each-scene-mesh (scene fu)
  (with-slots (mesh-table) scene
    (maphash fu mesh-table)))


(defmacro do-scene-meshes ((mesh id scene) &body body)
  `(for-each-scene-mesh ,scene (lambda (,id ,mesh) (declare (ignorable ,id)) ,@body)))


;;;
;;; SHOWCASE
;;;
(defclass pbr-showcase () ())


(defmethod initialize-instance :after ((this pbr-showcase) &key)
  (ge:mount-container "/bodge/demo/pbr/" (merge-showcase-pathname "pbr/assets/DamagedHelmet.brf")))


(register-showcase 'pbr-showcase)


(defmethod showcase-name ((this pbr-showcase))
  "PBR")


(defmethod showcase-revealing-flow ((this pbr-showcase) ui)
  (ge:for-graphics ()
    (setf *scene* (make-instance 'pbr-scene :resource (ge:load-resource "/bodge/demo/pbr/DamagedHelmet")
                                            :base-path "/bodge/demo/pbr/")
          *pbr-pipeline* (ge:make-shader-pipeline 'pbr-pipeline))))


(defmethod showcase-closing-flow ((this pbr-showcase))
  (ge:dispose *pbr-pipeline*)
  (ge:dispose *scene*))


(defmethod render-showcase ((this pbr-showcase))
  (ge:clear-rendering-output t :color (ge:vec4 0.2 0.2 0.2 1.0))
  (let* ((time (ge.util:epoch-seconds))
         (model-mat (ge:mult (ge:translation-mat4 0.3 0 -4)
                             (ge:euler-angles->mat4 (ge:vec3 (+ (/ pi 2) (/ (sin time) 2))
                                                             (/ (cos time) 4)
                                                             pi))))
         (view-mat (ge:identity-mat4))
         (view-model-mat (ge:mult view-mat model-mat))
         (mvp (ge:mult *projection-matrix*
                       view-model-mat))
         (normal-mat (ge:mat4->mat3 (ge:mult view-model-mat))))
    (do-scene-meshes (mesh id *scene*)
      (ge:render t *pbr-pipeline*
                 :primitive (primitive-of mesh)
                 :index-buffer (index-array-of mesh)
                 'position (position-array-of mesh)
                 'normal (normal-array-of mesh)
                 'tangent (tangent-array-of mesh)
                 'uv (tex-coord-array-of mesh)

                 'mvp mvp
                 'model-mat model-mat
                 'normal-mat normal-mat

                 'light-direction (ge:vec3 0 0 1)
                 'light-color (ge:vec3 1 1 1)
                 'camera (ge:vec3 0 0 0)

                 'base-color-factor (ge:vec4 1 1 1 1)
                 'base-color-sampler (scene-texture *scene* "Default_albedo.jpg")

                 'normal-sampler (scene-texture *scene* "Default_normal.jpg")
                 'normal-scale 1f0
                 'emissive-sampler (scene-texture *scene* "Default_emissive.jpg")
                 'emissive-factor (ge:vec3 0.7 0.7 0.7)
                 'metallic-roughness-sampler (scene-texture *scene* "Default_metalRoughness.jpg")
                 'metallic-roughness-values (ge:vec2 1 1)
                 'occlusion-sampler (scene-texture *scene* "Default_AO.jpg")
                 'occlusion-strength 1f0

                 'base-color-factor (ge:vec4 1 1 1 1)


                 'scale-diff-base *scale-diff-base*
                 'scale-fgd-spec *scale-fgd-spec*
                 'scale-ibl-ambient *scale-ibl-ambient*))))
