(cl:in-package :cl-bodge.demo)


(defclass demo (ge:enableable ge:generic-system)
  (ui
   window-width
   window-height
   pixel-ratio
   (active-showcase :initform nil)
   (next-showcase :initform nil))
  (:default-initargs :depends-on '(ge:host-system ge:physics-system
                                   ge:graphics-system ge:audio-system)))


(defun render (demo)
  (with-slots (ui active-showcase) demo
    (gl:clear-color 1.0 1.0 1.0 1.0)
    (gl:clear :color-buffer :stencil-buffer :depth-buffer)
    (when active-showcase
      (render-showcase active-showcase))
    (ge:compose-ui ui)
    (ge:swap-buffers)))


(ge:defwindow (main-menu
               (:title "Main Menu")
               (:width 150) (:height 480)
               (:options :scrollable)))


(defun init-graphics (this)
  (with-slots (ui window-width window-height pixel-ratio next-showcase) this
    (let ((input-source (ge:make-host-input-source)))
      (ge:attach-host-input-source input-source)
      (setf ui (ge:make-ui window-width window-height :input-source input-source
                                                      :pixel-ratio pixel-ratio))

      (let ((main-window (ge:add-window 'main-menu :ui ui :origin (ge:vec2 100 100))))
        (loop for case-class in (list-showcases)
              do (let ((showcase (make-instance case-class)))
                   (flet ((switch-showcase (win event)
                            (declare (ignore win event))
                            (setf next-showcase showcase)))
                     (let ((button (make-instance 'ge:button
                                                  :label (showcase-name showcase)
                                                  :on-click #'switch-showcase)))
                       (ge:adopt main-window button)))))))))


(defun init-host (this)
  (with-slots (window-width window-height pixel-ratio) this
    (let ((viewport-size (ge:viewport-size))
          (framebuffer-size (ge:framebuffer-size)))
      (setf (ge:swap-interval) 1
            window-width (ge:x viewport-size)
            window-height (ge:y viewport-size)
            pixel-ratio (ge.util:f (/ window-width (ge:x framebuffer-size)))))))


(defun cleanup-flow (this)
  (with-slots (active-showcase) this
    (prog1 (when active-showcase
             (showcase-closing-flow active-showcase))
      (setf active-showcase nil))))


(defun switch-showcase-if-requested (this)
  (with-slots (active-showcase next-showcase ui) this
    (when next-showcase
      (flow:serially
       (cleanup-flow this)
       (prog1 (showcase-revealing-flow next-showcase ui)
         (setf active-showcase next-showcase
               next-showcase nil))))))


(defmethod ge:initialize-system :after ((this demo))
  (ge:mount-container "/bodge/demo/" (merge-pathnames
                                      "assets/font.brf"
                                      (asdf:component-pathname (asdf:find-system :cl-bodge/demo))))
  (ge:run (flow:serially
           (ge:for-host ()
             (init-host this))
           (ge:for-graphics ()
             (init-graphics this))
           (ge:loop-flow
            (flow:serially
             (ge:for-graphics ()
               (render this))
             (flow:dynamically ()
               (switch-showcase-if-requested this)))
            (lambda () (ge:enabledp this)))
           (flow:dynamically ()
             (cleanup-flow this)))))


(ge:define-event-handler exit-handler ((eve ge:viewport-hiding-event))
  (ge:shutdown))


(defun run ()
  (ge:startup `(:engine (:systems (demo))
                :host (:opengl-version (3 3)))))
