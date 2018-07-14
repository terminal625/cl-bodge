(cl:defpackage :cl-bodge.pbr.demo
  (:use :cl :cl-bodge.demo.api))
(cl:in-package :cl-bodge.pbr.demo)


#++(ge:defsstruct pbr-info
  (n-dot-l :name "NdotL")
  (n-dot-v :name "NdotV")
  (n-dot-h :name "NdotH")
  (l-dot-h :name "LdotH")
  (v-dot-h :name "VdotH")
  (perceptual-roughness :name "perceptualRoughness")
  (metalness :name "metalness")
  (reflectance-0 :name "reflectance0")
  (reflectance-90 :name "reflectance90")
  (alpha-roughness :name "alphaRoughness")
  (diffuse-color :name "diffuseColor")
  (specular-color :name "specularColor"))


(ge:defshader (pbr-vert
               (:sources "pbr-vert.glsl")
               (:base-path :system-relative :cl-bodge/demo "cases/pbr"))
  (position :name "a_Position" :size 4)
  (normal :name "a_Normal" :size 4)
  (tangent :name "a_Tangent" :size 4)
  (uv :name "a_UV" :size 2)
  (mvp :name "u_MVPMatrix")
  (model-mat :name "u_ModelMatrix")
  (normal-mat "u_NormalMatrix"))


(ge:defshader (pbr-frag
               (:sources "pbr-frag.glsl")
               (:base-path :system-relative :cl-bodge/demo "cases/pbr"))
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



(ge:defpipeline pbr-pipeline
  :vertex pbr-vert
  :fragment pbr-frag)


(defvar *pbr-pipeline* nil)

;;;
;;; SHOWCASE
;;;
(defclass pbr-showcase () ())


(register-showcase 'pbr-showcase)



(defmethod showcase-name ((this pbr-showcase))
  "PBR")


(defmethod showcase-revealing-flow ((this pbr-showcase) ui)
  (ge:>>
   (ge:for-graphics ()
     (setf *pbr-pipeline* (ge:make-shader-pipeline 'pbr-pipeline)))))


(defmethod showcase-closing-flow ((this pbr-showcase))
  (ge:instantly ()
    (ge:dispose *pbr-pipeline*)))


(defmethod render-showcase ((this pbr-showcase))
  (ge:clear-rendering-output t :color (ge:vec4 0.2 0.2 0.2 1.0)))
