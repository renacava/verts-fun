;;;; verts-fun.asd

(asdf:defsystem #:verts-fun
  :description "Describe verts-fun here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :depends-on (#:cepl
               #:cepl.sdl2
               #:cepl.sdl2-image
               #:sdl2-ttf
               #:sdl2-image
               #:nineveh
               #:temporal-functions
               #:cepl.skitter.sdl2
               #:dirt
               #:cl-soil
               #:rtg-math
               #:sdl2-game-controller-db
               #:fiveam
               #:deploy)
  :build-operation "deploy-op"
  :build-pathname "verts-fun-deployed"
  :entry-point "verts-fun::start"
  :serial t
  :components ((:file "package")
               (:file "test")
               (:file "utilities")
               (:file "moduclass")
               (:file "load-file")
               (:file "keyboard")
               (:file "mouse")
               (:file "event")
               (:file "materials")
               (:file "cube")
               (:file "chunk")
               (:file "mesh")
               (:file "texture")
               (:file "text")
               (:file "world-query")
               (:file "window")
               (:file "camera")
               (:file "verts-fun")
               (:file "gui")
               ))
