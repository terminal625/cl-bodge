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
  (camera :name "u_Camera")
  ;; IBL
  (diffuse-env-sampler :name "u_DiffuseEnvSampler")
  (specular-env-sampler :name "u_SpecularEnvSampler")
  (brdf-lut :name "u_brdfLUT")
  ;; Base color
  (base-color-sampler :name "u_BaseColorSampler")
  (base-color-factor :name "u_BaseColorFactor")
  ;; Normal Mapping
  (normal-sampler :name "u_NormalSampler")
  (normal-scale :name "u_NormalScale")
  ;; Emissive Map
  (emissive-sampler :name "u_EmissiveSampler")
  (emissive-factor :name "u_EmissiveFactor")
  ;; Metallic-roughness
  (metallic-roughness-sampler :name "u_MetallicRoughnessSampler")
  (metallic-roughness-values :name "u_MetallicRoughnessValues")
  ;; Occulusion
  (occlusion-sampler :name "u_OcclusionSampler")
  (occlusion-strength :name "u_OcclusionStrength")
  ;; Debug params
  (scale-diff-base :name "u_ScaleDiffBaseMR")
  (scale-fgd-spec :name "u_ScaleFGDSpec")
  (scale-ibl-ambient :name "u_ScaleIBLAmbient"))


(defparameter *scale-diff-base* (ge:vec4 0.0 0.0 0.0 0.0))
(defparameter *scale-fgd-spec* (ge:vec4 0.0 0.0 0.0 0.0))
(defparameter *scale-ibl-ambient* (ge:vec4 0.2 0.2))


(ge:defpipeline pbr-pipeline
  :vertex pbr-vert
  :fragment pbr-frag)


(defvar *pbr-pipeline* nil)
(defvar *scene* nil)
(defvar *ibl-brdf-lut-tex* nil)
(defvar *ibl-diffuse-cubemap* nil)
(defvar *ibl-specular-cubemap* nil)

(defparameter *projection-matrix* (ge:perspective-projection-mat 1 (/ 600 800) 1 10))


;;;
;;; SHOWCASE
;;;
(defclass pbr-showcase () ())


(defmethod initialize-instance :after ((this pbr-showcase) &key)
  (ge:mount-container "/bodge/demo/pbr/helmet/" (merge-showcase-pathname "pbr/assets/DamagedHelmet.brf"))
  (ge:mount-filesystem "/bodge/demo/pbr/assets/" (merge-showcase-pathname "pbr/assets/")))


(register-showcase 'pbr-showcase)


(defmethod showcase-name ((this pbr-showcase))
  "PBR")


(defmethod showcase-revealing-flow ((this pbr-showcase) ui)
  (ge:for-graphics ()
    (setf *scene* (make-instance 'pbr-scene :resource (ge:load-resource "/bodge/demo/pbr/helmet/DamagedHelmet")
                                            :base-path "/bodge/demo/pbr/helmet/")
          *ibl-brdf-lut-tex* (load-brdf-texture)
          *ibl-diffuse-cubemap* (load-diffuse-ibl-cubemap)
          *ibl-specular-cubemap* (load-specular-ibl-cubemap)
          *pbr-pipeline* (ge:make-shader-pipeline 'pbr-pipeline))))


(defmethod showcase-closing-flow ((this pbr-showcase))
  (ge:dispose *pbr-pipeline*)
  (ge:dispose *scene*)
  (ge:dispose *ibl-brdf-lut-tex*)
  (ge:dispose *ibl-diffuse-cubemap*)
  (ge:dispose *ibl-specular-cubemap*))


(defmethod render-showcase ((this pbr-showcase))
  (ge:clear-rendering-output t :color (ge:vec4 0.2 0.2 0.2 1.0))
  (let* ((time (ge.util:epoch-seconds))
         (model-mat (ge:mult (ge:translation-mat4 0.3 0 -4)
                             (ge:euler-angles->mat4 (ge:vec3 (+ (/ pi 2) (/ (sin time) 2))
                                                             0
                                                             (+ pi (/ (cos time) 0.5))))))
         (view-mat (ge:identity-mat4))
         (view-model-mat (ge:mult view-mat model-mat))
         (mvp (ge:mult *projection-matrix*
                       view-model-mat))
         (normal-mat (ge:inverse (ge:transpose (ge:mat4->mat3 (ge:mult view-model-mat))))))
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

                 'light-direction (ge:vec3 0 1 0)
                 'light-color (ge:vec3 1.0 1.0 1.0)
                 'camera (ge:vec3 0 0 0)

                 'base-color-factor (ge:vec4 1.0 1.0 1.0 1.0)
                 'base-color-sampler (scene-texture *scene* "Default_albedo.jpg")

                 'normal-sampler (scene-texture *scene* "Default_normal.jpg")
                 'normal-scale 1f0

                 'emissive-sampler (scene-texture *scene* "Default_emissive.jpg")
                 'emissive-factor (ge:vec3 0.7 0.7 0.7)

                 'metallic-roughness-sampler (scene-texture *scene* "Default_metalRoughness.jpg")
                 'metallic-roughness-values (ge:vec2 1.0 1.0)

                 'occlusion-sampler (scene-texture *scene* "Default_AO.jpg")
                 'occlusion-strength 1f0

                 'diffuse-env-sampler *ibl-diffuse-cubemap*
                 'specular-env-sampler *ibl-specular-cubemap*
                 'brdf-lut *ibl-brdf-lut-tex*

                 'scale-diff-base *scale-diff-base*
                 'scale-fgd-spec *scale-fgd-spec*
                 'scale-ibl-ambient *scale-ibl-ambient*))))
