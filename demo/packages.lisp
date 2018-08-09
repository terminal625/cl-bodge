(cl:defpackage :cl-bodge.demo.api
  (:use :cl)
  (:export #:*viewport-pixel-ratio*
           #:*viewport-scale*
           #:*loading-screen-canvas*
           #:render-loading-screen
           #:register-showcase
           #:list-showcases
           #:showcase-name
           #:showcase-class
           #:showcase-revealing-flow
           #:showcase-closing-flow
           #:render-showcase
           #:merge-showcase-pathname))


(cl:defpackage :cl-bodge.demo
  (:use :cl :cl-bodge.demo.api)
  (:export #:run))
