(cl:in-package :cl-bodge.demo)


(defclass custom-button (ge:custom-widget) ())


(defmethod ge:render-custom-widget ((this custom-button) origin width height)
  (ge:draw-rect origin width height
                :fill-paint (cond
                              ((and (ge:custom-widget-hovered-p this)
                                    (ge:custom-widget-pressed-p this :left))
                               (ge:vec4 0.3 0.3 0.3 1.0))
                              ((ge:custom-widget-hovered-p this) (ge:vec4 0.5 0.5 0.5 1.0))
                              (t (ge:vec4 0.0 0.0 0.0 1.0))))
  (ge:draw-text (ge:add origin (ge:vec2 12 9)) "Hello Widget" :fill-color (ge:vec4 1.0 1.0 1.0 1.0)))



(let ((output *standard-output*))
  (defun on-hover (window event)
    (declare (ignore window event))
    (format output "~&hovering"))
  (defun on-leave (window event)
    (declare (ignore window event))
    (format output "~&leaving"))
  (defun on-click (window event)
    (declare (ignore window))
    (format output "~&~A clicked" (ge:button-from event)))
  (defun on-mouse-press (window event)
    (declare (ignore window))
    (format output "~&~A pressed" (ge:button-from event)))
  (defun on-mouse-release (window event)
    (declare (ignore window))
    (format output "~&~A released" (ge:button-from event))))

(ge:defwindow (ui-demo-window
               (:title "UI Demo")
               (:origin 200 50)
               (:width 400) (:height 400)
               (:options :movable :resizable
                         :minimizable :scrollable
                         :closable))
  (ge:label :text "Nested:")
  (ge:horizontal-layout
   (ge:vertical-layout
    (ge:radio :label "Option 1")
    (ge:radio :label "Option 2"))
   (ge:vertical-layout
    (ge:check-box :label "Check 1" :width 100)
    (ge:check-box :label "Check 2"))
   (ge:vertical-layout
    (ge:label :text "Awesomely" :align :left)
    (ge:label :text "Stacked" :align :middle)
    (ge:label :text "Labels" :align :right)))
  (ge:label :text "Expand by width:")
  (ge:horizontal-layout
     (ge:button :label "Dynamic")
     (ge:button :label "Min-Width" :width 80)
     (ge:button :label "Fixed-Width" :expandable nil :width 100))
  (ge:label :text "Expand by ratio:")
  (ge:horizontal-layout
     (ge:button :label "1.0" :expand-ratio 1.0)
     (ge:button :label "0.75" :expand-ratio 0.75)
     (ge:button :label "0.5" :expand-ratio 0.5))
  (ge:label :text "Rest:")
  (ge:button :label "Top-Level Button")
  (custom-button :on-hover #'on-hover
                 :on-leave #'on-leave
                 :on-click #'on-click
                 :on-mouse-press #'on-mouse-press
                 :on-mouse-release #'on-mouse-release))


(defmethod ge:on-window-close ((this ui-demo-window))
  (ge:remove-window this))


(defun open-ui-demo-window (menu event)
  (declare (ignore menu event))
  (ge:add-window 'ui-demo-window))
