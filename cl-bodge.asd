(asdf:defsystem cl-bodge/utils
  :description "Bodacious Game Engine various utilities"
  :version "0.4.0"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :depends-on (alexandria uiop log4cl local-time dissect split-sequence cffi
                          claw static-vectors trivial-gray-streams)
  :pathname "utils/"
  :serial t
  :components ((:file "packages")
               (:file "utils")
               (:file "language")
               (:file "collections")
               (:file "foreign")
               (:file "strings")
               (:file "trees")
               (:file "arrays")
               (:file "streams")))


(asdf:defsystem cl-bodge/engine
  :description "Bodacious Game Engine foundation library"
  :version "0.4.0"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :depends-on (cl-bodge/utils cl-muth rtg-math log4cl bordeaux-threads local-time
                              bodge-blobs-support trivial-garbage uiop cffi cl-flow uiop
                              trivial-main-thread simple-flow-dispatcher claw)
  :pathname "engine/"
  :serial t
  :components ((:file "packages")
               (:module math
                        :serial t
                        :components ((:file "types")
                                     (:file "scalar")
                                     (:file "vector")
                                     (:file "matrix")
                                     (:file "quaternion")))
               (:module memory
                        :serial t
                        :components ((:file "disposable")
                                     (:file "foreign-array")))
               (:module concurrency
                        :serial t
                        :components ((:file "dispatch")
                                     (:file "execution")
                                     (:file "task-queue")
                                     (:file "instance-lock")
                                     (:file "main-thread")))
               (:module resources
                        :components ((:file "audio")
                                     (:file "graphics")))
               (:module events
                        :components ((:file "event")
                                     (:file "emitter")
                                     (:file "listener")
                                     (:file "hub")))
               (:file "properties")
               (:file "engine")
               (:file "handle")
               (:file "event")
               (:file "generic-system")
               (:file "thread-bound-system")))


(asdf:defsystem cl-bodge/resources
  :description "Bodacious Game Engine resource management"
  :version "0.4.0"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :depends-on (cl-bodge/utils cl-bodge/engine flexi-streams
               opticl cl-fad chipz log4cl static-vectors
               sndfile-blob bodge-sndfile cl-conspack)
  :pathname "resources/"
  :serial t
  :components ((:file "packages")
               (:file "storage")
               (:file "handler")
               (:file "registry")
               (:module chunked :serial t
                :components ((:file "resource")
                             (:file "container")
                             (:file "chunk-structure")
                             (:file "font")))
               (:file "audio")
               (:file "image")
               (:file "font")
               (:module scene :serial t
                :components ((:file "scene")
                             (:file "read")
                             (:file "write")
                             (:file "handler")))))


(asdf:defsystem cl-bodge/host
  :description "Bodacious Game Engine host system"
  :version "0.4.0"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :depends-on (cl-bodge/engine cl-bodge/utils log4cl bordeaux-threads
                               glfw-blob bodge-glfw cl-muth)
  :pathname "host/"
  :serial t
  :components ((:file "packages")
               (:file "events")
               (:file "system")
               (:file "hotkey")
               (:file "input-map")))


(asdf:defsystem cl-bodge/graphics
  :description "Bodacious Game Engine graphics system"
  :version "0.4.0"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :depends-on (cl-bodge/engine cl-bodge/utils cl-bodge/host cl-bodge/resources
                               cl-opengl log4cl local-time cffi
                               glad-blob bodge-glad
                               static-vectors)
  :pathname "graphics/"
  :serial t
  :components ((:file "packages")
               (:file "utils")
               (:file "state")
               (:module "input" :components ((:file "common")
                                             (:file "buffers")
                                             (:file "array-buffer")
                                             (:file "index-buffer")
                                             (:file "uniforms")
                                             (:file "structs")
                                             (:file "textures")))
               (:module "output" :components ((:file "output")
                                              (:file "default")
                                              (:file "textures")
                                              (:file "renderbuffer")
                                              (:file "attachments")
                                              (:file "framebuffer")))
               (:file "shader-registry")
               (:file "shader")
               (:file "pipeline")
               (:file "rendering")
               (:file "system")))


(asdf:defsystem cl-bodge/canvas
  :description "Bodacious Game Engine vector graphics system"
  :version "0.4.0"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :depends-on (cl-bodge/engine cl-bodge/utils cl-bodge/graphics
                               cl-bodge/resources claw log4cl
                               static-vectors asdf
                               nanovg-blob bodge-nanovg)
  :pathname "canvas/"
  :serial t
  :components ((:file "packages")
               (:file "canvas")
               (:file "transform")
               (:file "image")
               (:file "paint")
               (:file "primitives")
               (:file "text")))


(asdf:defsystem cl-bodge/animation
  :description "Bodacious Game Engine animation library"
  :version "0.4.0"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :depends-on (cl-bodge/engine cl-bodge/utils cl-bodge/resources cl-bodge/graphics)
  :pathname "animation/"
  :serial t
  :components ((:file "packages")
               (:file "keyframed")
               (:file "stream")
               (:file "resources")))


(asdf:defsystem cl-bodge/audio
  :description "Bodacious Game Engine audio system"
  :version "0.4.0"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :depends-on (cl-bodge/engine cl-bodge/utils cl-bodge/host log4cl
                               openal-blob bodge-openal)
  :pathname "audio/"
  :serial t
  :components ((:file "packages")
               (:file "al")
               (:file "buffer")
               (:file "source")
               (:file "system")))


(asdf:defsystem cl-bodge/physics/backend
  :description "Bodacious Game Engine physics system"
  :version "0.4.0"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :pathname "physics/backend"
  :serial t
  :components ((:file "packages")
               (:file "backend")))


(asdf:defsystem cl-bodge/physics
  :description "Bodacious Game Engine physics system"
  :version "0.4.0"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :depends-on (cl-bodge/utils cl-bodge/engine cl-bodge/physics/backend)
  :pathname "physics/"
  :serial t
  :components ((:file "packages")
               (:file "api")
               (:file "system")))


(asdf:defsystem cl-bodge/physics/3d
  :description "Bodacious Game Engine physics system"
  :version "0.4.0"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :depends-on (cl-bodge/utils cl-bodge/engine cl-bodge/physics/backend
                              ode-blob bodge-ode log4cl claw local-time)
  :pathname "physics/3d/"
  :serial t
  :components ((:file "packages")
               (:file "common")
               (:module ode :components ((:file "ode")
                                         (:file "contacts")
                                         (:file "universe")
                                         (:file "mass")
                                         (:file "rigid-body")
                                         (:file "joints")
                                         (:file "geometry")))
               (:file "engine")
               (:file "universe")
               (:file "mass")
               (:file "contact")
               (:file "rigid-body")
               (:file "shape")))


(asdf:defsystem cl-bodge/physics/2d
  :description "Bodacious Game Engine physics system"
  :version "0.4.0"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :depends-on (cl-bodge/utils cl-bodge/engine cl-bodge/physics/backend
                              chipmunk-blob bodge-chipmunk log4cl claw
                              trivial-garbage cffi)
  :pathname "physics/2d/"
  :serial t
  :components ((:file "packages")
               (:file "utils")
               (:file "chipmunk")
               (:file "contact")
               (:file "universe")
               (:file "body")
               (:file "shape")))


(asdf:defsystem cl-bodge/shading
  :description "Bodacious Game Engine shading library"
  :version "0.4.0"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :depends-on (cl-bodge/engine cl-bodge/utils cl-bodge/graphics)
  :pathname "shading/"
  :serial t
  :components ((:file "packages")
               (:file "math/math")
               (:file "shadow/shadow")
               (:file "phong/phong")
               (:file "banner/banner")
               (:file "skinning/skinning")))


(asdf:defsystem cl-bodge/text
  :description "Bodacious Game Engine text rendering"
  :version "0.4.0"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :depends-on (cl-bodge/engine cl-bodge/utils cl-bodge/resources
                               cl-bodge/graphics
                               log4cl)
  :pathname "text/"
  :serial t
  :components ((:file "packages")
               (:file "font")
               (:file "text")
               (:file "rated-queue")
               (:file "text-cache")
               (:module shaders :components ((:file "text")))
               (:file "text-renderer")
               (:file "resources")))


(asdf:defsystem cl-bodge/ui
  :description "Bodacious Game Engine Plain Old Interface for Users"
  :version "0.4.0"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :depends-on (cl-bodge/engine cl-bodge/utils cl-bodge/graphics cl-bodge/canvas
                               cl-bodge/host nuklear-blob bodge-nuklear claw)
  :pathname "ui/"
  :serial t
  :components ((:file "packages")
               (:file "input-source")
               (:file "ui")
               (:file "events")
               (:file "style")
               (:file "elements")
               (:file "rendering")))


(asdf:defsystem cl-bodge/distribution
  :description "Bodacious Game Engine distribution helpers"
  :version "0.4.0"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :depends-on (cl-bodge/utils cl-bodge/resources asdf uiop cl-fad cffi cl-ppcre
                              bodge-blobs-support inferior-shell split-sequence flexi-streams
                              trivial-features)
  :pathname "distribution/"
  :serial t
  :components ((:file "packages")
               (:file "utils")
               (:file "distribution")
               (:file "registry")
               (:module darwin
                        :if-feature :darwin
                        :components ((:file "build")))
               (:module unix
                        :if-feature (:and :unix (:not :darwin))
                        :components ((:file "build")))
               (:module windows
                        :if-feature (:or :windows :win32)
                        :components ((:file "build")))
               (:file "build-unknown" :if-feature (:not (:or :darwin :unix :win32 :windows)))
               (:file "build")))


(asdf:defsystem cl-bodge/tests
  :description "Test suite for cl-bodge engine"
  :version "0.4.0"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :depends-on (cl-bodge/utils cl-bodge/text flexi-streams fiveam)
  :pathname "t/"
  :serial t
  :components ((:file "packages")
               (:file "suites")
               (:file "rated-queue")
               (:file "circular-buffer")
               (:file "buffered-output-stream")))


(asdf:defsystem cl-bodge
  :description "Bodacious Game Engine framework"
  :version "0.4.0"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :depends-on (cl-bodge/engine cl-bodge/utils cl-bodge/host cl-bodge/graphics cl-bodge/audio
                               cl-bodge/physics cl-bodge/physics/2d cl-bodge/physics/3d
                               cl-bodge/resources cl-bodge/ui cl-bodge/text
                               cl-bodge/canvas cl-bodge/animation
                               cl-bodge/shading)
  :components ((:file "packages")))


(asdf:defsystem cl-bodge/demo
  :description "cl-bodge demo to check if all systems work as expected"
  :version "1.0.0"
  :author "Pavel Korolev"
  :license "MIT"
  :depends-on (cl-bodge)
  :pathname "demo/"
  :serial t
  :components ((:file "packages")
               (:file "utils")
               (:file "scene")
               (:file "case")
               (:module cases
                        :serial t
                        :components ((:file "2d-physics")
                                     (:file "3d-physics")
                                     (:file "framebuffers/framebuffers")
                                     (:file "ui")
                                     (:file "text/text")
                                     (:file "pbr/pbr")))
               (:file "demo")))
