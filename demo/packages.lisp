(cl:defpackage :cl-bodge.demo.api
  (:use :cl)
  (:export #:register-showcase
           #:list-showcases
           #:showcase-name
           #:showcase-revealing-flow
           #:showcase-closing-flow
           #:render-showcase))


(cl:defpackage :cl-bodge.demo
  (:use :cl :cl-bodge.demo.api)
  (:export #:run))
