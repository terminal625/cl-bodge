(cl:in-package :cl-bodge.demo.api)

(defvar *viewport-pixel-ratio* 1f0)
(defvar *viewport-scale* 1f0)
(defvar *loading-screen-canvas*)

(defun merge-showcase-pathname (pathname)
  (merge-pathnames pathname
                   (merge-pathnames "cases/"
                                    (asdf:component-pathname (asdf:find-system :cl-bodge/demo)))))


(defvar *showcases* nil)

(defun register-showcase (class)
  (pushnew class *showcases*))

(defun list-showcases ()
  (reverse *showcases*))

(defgeneric showcase-name (case-manager))

(defgeneric showcase-revealing-flow (case-manager ui))
(defgeneric showcase-closing-flow (case-manager))

(defgeneric render-showcase (case-manager)
  (:method (case-manager) (declare (ignore case-manager))))


(defun render-loading-screen (color)
  (ge:render t *loading-screen-canvas* :color color))
