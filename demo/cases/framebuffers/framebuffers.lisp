(cl:defpackage :cl-bodge.physics.framebuffers.demo
  (:use :cl :cl-bodge.demo.api :cl-bodge.demo.scene))
(cl:in-package :cl-bodge.physics.framebuffers.demo)


(defclass framebuffers-showcase () ())


(register-showcase 'framebuffers-showcase)


(defmethod showcase-name ((this framebuffers-showcase))
  "Framebuffers")


(defvar *box* nil)
(defvar *box-pipeline* nil)

(defvar *scene-1* nil)
(defvar *scene-2* nil)

(defvar *framebuffer* nil)

(defvar *banner-pipeline* nil)
(defvar *banner-pos* nil)
(defvar *banner-tex-coords* nil)

(defvar *2d-texture* nil)

(defvar *depth-texture* nil)
(defvar *depth-pipeline* nil)

(defvar *cubemap-texture* nil)
(defvar *depth-cubemap-texture* nil)

(defparameter *view-matrix* (ge:translation-mat4 0 0 -1))
(defparameter *projection-matrix* (ge:perspective-projection-mat 1 (/ 480 640) 1 10))


(ge:defshader (framebuffers-shader
               (:sources "framebuffers.glsl")
               (:base-path :system-relative :cl-bodge/demo "cases/framebuffers"))
  (position :name "vPosition")
  (color :name "diffuseColor")
  (cube-texture :name "cubeMap")
  (model :name "model")
  (view :name "view")
  (projection :name "projection"))


(ge:defpipeline (framebuffers-pipeline
                 (:primitive :triangles))
  :vertex framebuffers-shader
  :fragment framebuffers-shader)


(defclass box (ge:disposable)
  ((position-buffer :initarg :position-buffer)
   (index-buffer :initarg :index-buffer)
   (color :initform (ge:vec3 0.9 0.9 0.9))))


(ge:define-destructor box (position-buffer index-buffer)
  (ge:dispose position-buffer)
  (ge:dispose index-buffer))


(defun render-box (box output transform)
  (with-slots (position-buffer index-buffer color) box
    (ge:render output *box-pipeline*
               :index-buffer index-buffer
               'position position-buffer
               'model transform
               'view *view-matrix*
               'cube-texture *cubemap-texture*
               'projection *projection-matrix*
               'color color)))


(defun make-box (x y z)
  (multiple-value-bind (vertices normals indices)
      (cl-bodge.demo::generate-box-arrays x y z)
    (declare (ignore normals))
    (make-instance 'box :index-buffer (ge:make-index-buffer indices)
                        :position-buffer (ge:make-array-buffer vertices :element-size 3))))


(defmethod showcase-revealing-flow ((this framebuffers-showcase) ui)
  (ge:for-graphics ()
    (setf *box* (make-box 1 1 1)
          *box-pipeline* (ge:make-shader-pipeline 'framebuffers-pipeline)
          *scene-1* (make-simple-scene)
          *scene-2* (make-simple-scene)
          *framebuffer* (ge:make-framebuffer)
          *2d-texture* (ge:make-empty-2d-texture 640 480 :rgba)
          *banner-pipeline* (ge:make-shader-pipeline 'ge:banner-pipeline)
          *banner-pos* (ge:make-array-buffer #2a((1 -1 0)
                                                 (1 1 0)
                                                 (-1 -1 0)
                                                 (-1 1 0)))
          *banner-tex-coords* (ge:make-array-buffer #2a((1 0)
                                                        (1 1)
                                                        (0 0)
                                                        (0 1)))
          *depth-texture* (ge:make-empty-depth-texture 640 480)
          *depth-pipeline* (ge:make-shader-pipeline 'ge:depth-pipeline)

          *cubemap-texture* (ge.gx:make-empty-cubemap-texture 1024 :rgba)
          *depth-cubemap-texture* (ge.gx:make-empty-depth-cubemap-texture 1024))
    (ge.gx:configure-framebuffer *framebuffer* *2d-texture* *depth-texture*)))


(defmethod showcase-closing-flow ((this framebuffers-showcase))
  (ge:for-graphics ()
    (loop for element in (list *box*
                               *box-pipeline*
                               *scene-1*
                               *scene-2*
                               *framebuffer*
                               *banner-pipeline*
                               *2d-texture*
                               *depth-texture*
                               *depth-pipeline*
                               *cubemap-texture*
                               *depth-cubemap-texture*)
          do (ge:dispose element))))


(defmethod render-showcase ((this framebuffers-showcase))
  (ge:clear-rendering-output t :color (ge:vec4 0.1 0.1 0.1 1.0))
  (ge:clear-rendering-output (ge:cubemap-positive-x-layer *cubemap-texture*)
                             :color (ge:vec4 0.1 0.3 0.1 1.0))
  (ge:clear-rendering-output (ge:cubemap-positive-y-layer *cubemap-texture*)
                             :color (ge:vec4 0.3 0.1 0.1 1.0))
  (ge:clear-rendering-output (ge:cubemap-positive-z-layer *cubemap-texture*)
                             :color (ge:vec4 0.1 0.1 0.3 1.0))
  (ge:clear-rendering-output (ge:cubemap-negative-x-layer *cubemap-texture*)
                             :color (ge:vec4 0.3 0.3 0.1 1.0))
  (ge:clear-rendering-output (ge:cubemap-negative-y-layer *cubemap-texture*)
                             :color (ge:vec4 0.1 0.3 0.3 1.0))
  (ge:clear-rendering-output (ge:cubemap-negative-z-layer *cubemap-texture*)
                             :color (ge:vec4 0.3 0.1 0.3 1.0))


  (let ((time (float (ge.util:real-time-seconds) 0f0)))
    (render-box *box* t (ge:mult (ge:translation-mat4 0.3 0 -2)
                                 (ge:euler-angles->mat4 (ge:vec3 (* 0.3 time)
                                                                 0
                                                                 (* 0.3 time))))))
  #++(progn
    (let ((position (ge:vec3 (* 1.2 (sin time))
                             (* 1.2 (cos time))
                             (* (cos time) (sin time)))))
      (update-light scene :position position)
      (transform-object bulb position (ge:vec3)))
    (ge:clear-rendering-output *tex* :color (ge:vec4 0.2 0.3 0.3 1.0))
    (ge:clear-rendering-output *depth*)
    (ge:clear-rendering-output *cubemap*)
    (render-scene scene *framebuffer*)
    (ge:render t *banner-pipeline*
               :vertex-count 4
               :primitive :triangle-strip
               'ge:banner-mvp (ge:mult (ge:perspective-projection-mat 1 1 1 10)
                                       (ge:translation-mat4 (* 0.4 (sin time))
                                                            (* 0.4 (cos time)) -4))
               'ge:banner-position *banner-pos*
               'ge:banner-tex-coord *banner-tex*
               'ge:banner-texture *tex*)))
