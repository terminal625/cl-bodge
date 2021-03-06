(cl:in-package :cl-bodge.host)

(declaim (special *window*))

(define-constant +expected-dpi+ 96)

(defclass host-application (bodge-host:window)
  ((init-continuation :initarg :init-continuation)
   (destroy-continuation :initarg :destroy-continuation)))


(defun set-destroy-continuation (host-application cont)
  (with-slots (destroy-continuation) host-application
    (setf destroy-continuation cont)))


(defclass host-system (enableable dispatching generic-system)
  ((host-application :initform nil)
   (shared :initform nil)))


(definline host-application (&optional (system *system*))
  (slot-value system 'host-application))


(definline shared-application (&optional (system *system*))
  (slot-value system 'shared))


(definline host ()
  (engine-system 'host-system))


(defmacro for-host ((&optional arg) &body body)
  `(ge.ng:-> (host) (,@(when arg (list arg)))
     ,@body))


(defmethod dispatch ((this host-system) (fn function) invariant &key)
  (bodge-host:progm
    (let ((*system* this))
      (funcall fn)))
  t)


(defmethod bodge-host:on-init ((this host-application))
  (with-slots (init-continuation) this
    (run (concurrently ()
           (funcall init-continuation)))))


(defmethod bodge-host:on-destroy ((this host-application))
  (with-slots (destroy-continuation) this
    (run (concurrently ()
           (funcall destroy-continuation)))))


(defmethod bodge-host:on-hide ((this host-application))
  (post 'viewport-hiding-event))


(defmethod bodge-host:on-key-action ((this host-application) key state)
  (post 'keyboard-event :key key :state state))


(defmethod bodge-host:on-mouse-action ((this host-application) button state)
  (post 'mouse-event :button button :state state))


(defmethod bodge-host:on-cursor-movement ((this host-application) x y)
  (post 'cursor-event :x x :y y))


(defmethod bodge-host:on-scroll ((this host-application) x y)
  (post 'scroll-event :x-offset x :y-offset y))


(defmethod bodge-host:on-framebuffer-size-change ((this host-application) w h)
  (post 'framebuffer-size-change-event :width w :height h))


(defmethod bodge-host:on-viewport-size-change ((this host-application) w h)
  (post 'viewport-size-change-event :width w :height h))


(defmethod bodge-host:on-character-input ((this host-application) character)
  (post 'character-input-event :character character))


(defun make-host-application (cont)
  (make-instance 'host-application
                 :init-continuation cont
                 :opengl-version (property '(:host :opengl-version) '(3 3))
                 :resizable (property '(:host :viewport-resizable) nil)
                 :decorated (property '(:host :viewport-decorated) t)
                 :transparent (property '(:host :viewport-transparent) nil)
                 :samples (property '(:host :samples) nil)))


(defmethod enabling-flow list ((this host-system))
  (with-slots (host-application shared) this
    (>> (%> ()
          (setf host-application (make-host-application #'ge.ng:continue-flow))
          (bodge-host:open-window (host-application this)))
        (instantly ()
          (setf shared (bodge-host:make-shared-rendering-context host-application))
          (log:debug "Host system initialized")))))


(defmethod disabling-flow list ((this host-system))
  (>>
   (%> ()
     (bodge-host:destroy-shared-rendering-context (shared-application this))
     (set-destroy-continuation (host-application this) #'continue-flow)
     (bodge-host:close-window (host-application this)))
   (instantly ()
     (log:debug "Host system offline"))))


(defun bind-rendering-context (&key (main t))
  (let ((host (host)))
    (if main
        (bodge-host:bind-main-rendering-context (host-application host))
        (bodge-host:bind-shared-rendering-context (shared-application host)))))


(defun release-rendering-context ()
  (bodge-host:release-rendering-context))


(defun swap-buffers ()
  (bodge-host:swap-buffers (host-application (host))))


(defun swap-interval ()
  (bodge-host:swap-interval))


(defun (setf swap-interval) (value)
  (setf (bodge-host:swap-interval) value))


(define-system-function (setf viewport-title) host-system (value)
  (setf (bodge-host:viewport-title (host-application)) value))


(define-system-function viewport-size host-system ()
  (bodge-host:viewport-size (host-application)))


(define-system-function framebuffer-size host-system ()
  (bodge-host:framebuffer-size (host-application)))


(define-system-function (setf viewport-size) host-system (value)
  (setf (bodge-host:viewport-size (host-application)) value))


(define-system-function cursor-position host-system (&optional (result-vec (vec2)))
  (bodge-host:cursor-position (host-application) result-vec))


(define-system-function mouse-button-state host-system (button)
  (bodge-host:mouse-button-state (host-application) button))


(define-system-function keyboard-button-state host-system (button)
  (bodge-host:keyboard-button-state (host-application) button))


(define-system-function lock-cursor host-system ()
  (bodge-host:lock-cursor (host-application)))


(define-system-function unlock-cursor host-system ()
  (bodge-host:unlock-cursor (host-application)))


(define-system-function viewport-scale host-system ()
  (bodge-host:viewport-scale (host-application)))


(define-system-function (setf fullscreen-viewport-p) host-system (value)
  (setf (bodge-host:fullscreen-viewport-p (host-application)) value))
