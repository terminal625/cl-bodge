(cl:defpackage :cl-bodge.demo
  (:use :cl)
  (:export #:run))
(cl:in-package :cl-bodge.demo)


(defclass demo (ge:enableable ge:generic-system)
  (ui
   canvas
   window-width
   window-height
   pixel-ratio)
  (:default-initargs :depends-on '(ge:host-system ge:graphics-system ge:audio-system)))


(defun render (demo)
  (with-slots (canvas ui) demo
    (gl:clear-color 1.0 1.0 1.0 1.0)
    (gl:clear :color-buffer)
    (ge:with-canvas (canvas)
      (ge:draw-text (ge:vec2 100 100) "HOLY SHIT"))
    (ge:compose-ui ui)
    (ge:swap-buffers)))


(defclass custom (ge:custom-widget) ())


(defmethod ge:render-widget ((this custom) origin width height)
  (ge:draw-rect origin width height :fill-paint (ge:vec4 0.0 0.0 0.0 1.0))
  (ge:draw-text (ge:add origin (ge:vec2 12 9)) "Hello Widget" :fill-color (ge:vec4 1.0 1.0 1.0 1.0)))


(let ((output *standard-output*))
  (defmethod ge:update-widget ((this custom))
    (format output "~&update called")))


(ge:defwindow (main-menu
               (:title "Main Menu")
               (:width 150) (:height 480)
               (:options :scrollable :resizable))
  (ge:horizontal-layout
   (ge:vertical-layout
    (ge:option :label "Option 1")
    (ge:option :label "Option 2"))
   (ge:vertical-layout
    (ge:label :text "yo")
    (ge:label :text "hallo")
    (ge:label :text "there")))
  (ge:button :label "Audio")
  (ge:button :label "3D Physics")
  (ge:button :label "2D Physics")
  (custom))


(defun init-graphics (this)
  (with-slots (ui canvas window-width window-height pixel-ratio) this
    (let ((input-source (ge:make-host-input-source)))
      (ge:attach-host-input-source input-source)
      (setf canvas (ge:make-canvas window-width window-height)
            ui (ge:make-ui window-width window-height :input-source input-source
                                                      :pixel-ratio pixel-ratio))
      (ge:add-window 'main-menu ui :origin (ge:vec2 100 100)))))


(defun init-host (this)
  (with-slots (window-width window-height pixel-ratio) this
    (let ((viewport-size (ge:viewport-size))
          (framebuffer-size (ge:framebuffer-size)))
      (setf (ge:swap-interval) 1
            window-width (ge:x viewport-size)
            window-height (ge:y viewport-size)
            pixel-ratio (ge.util:f (/ window-width (ge:x framebuffer-size)))))))


(defmethod ge:initialize-system :after ((this demo))
  (ge:run (flow:serially
           (ge:for-host ()
             (init-host this))
           (ge:for-graphics ()
             (init-graphics this))
           (ge:loop-flow
            (flow:serially
             (ge:for-graphics ()
               (render this)))
            (lambda () (ge:enabledp this))))))


(ge:define-event-handler exit-handler ((eve ge:viewport-hiding-event))
  (ge:shutdown))


(defun run ()
  (ge:startup `(:engine (:systems (demo)))))
