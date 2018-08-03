(cl:defpackage :cl-bodge.pbr.demo
  (:use :cl :cl-bodge.demo.api :cl-bodge.demo.scene))
(cl:in-package :cl-bodge.pbr.demo)


(ge:defshader (pbr-vert
               (:sources "pbr-vert.glsl")
               (:base-path (merge-showcase-pathname "pbr/")))
  (position :name "a_Position" :size 4)
  (normal :name "a_Normal" :size 4)
  (tangent :name "a_Tangent" :size 4)
  (uv :name "a_UV" :size 2)
  (mvp :name "u_MVPMatrix")
  (model-mat :name "u_ModelMatrix")
  (normal-mat "u_NormalMatrix"))


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


(ge:defpipeline pbr-pipeline
  :vertex pbr-vert
  :fragment pbr-frag)


(defvar *pbr-pipeline* nil)
(defvar *model* nil)


(defvar *scene* nil)
(defvar *mesh* nil)
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
  (ge:>>
   (ge:instantly ()
     (setf *model* (ge:load-resource "/bodge/demo/pbr/DamagedHelmet")
           *scene* (make-simple-scene))
     (let ((mesh (ge:scene-resource-mesh *model* 0)))
       (setf *mesh* (add-mesh *scene*
                              (ge:mesh-resource-position-array mesh)
                              (ge:mesh-resource-index-array mesh)
                              (ge:mesh-resource-normal-array mesh)
                              (ge:mesh-resource-primitive mesh)))))
   (ge:for-graphics ()
     (setf *pbr-pipeline* (ge:make-shader-pipeline 'pbr-pipeline)))))


(defmethod showcase-closing-flow ((this pbr-showcase))
  (ge:>> (ge:instantly ()
           (ge:dispose *pbr-pipeline*))
         (ge:for-graphics ()
           (ge:dispose *scene*))))


(defmethod render-showcase ((this pbr-showcase))
  (ge:clear-rendering-output t :color (ge:vec4 0.2 0.2 0.2 1.0))
  (let ((time (ge.util:real-time-seconds)))
    (update-drawable *mesh* :transform (ge:mult
                                        (ge:translation-mat4 (sin time) (cos time) -5)
                                        (ge:euler-angles->mat4 (ge:vec3 (/ pi 2) 0 pi)))))
  (render-scene *scene*))
