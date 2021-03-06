(cl:in-package :cl-bodge.shading)


(defsstruct phong-point-light
  (position :vec3)
  (color :vec3)
  (ambient :vec3)
  (falloff :float)
  (radius :float))


(defsstruct phong-material
  (specular-scale :float :name "specularScale")
  (shininess :float)
  (roughness :float)
  (albedo :float))


(defshader (phong-shader
            (:name "bodge/phong")
            (:headers "phong.h")
            (:sources "phong.glsl")
            (:base-path :system-relative :cl-bodge/shading "phong/")))
