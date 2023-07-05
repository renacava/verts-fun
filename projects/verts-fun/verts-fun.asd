;;;; verts-fun.asd

(asdf:defsystem #:verts-fun
  :description "Describe verts-fun here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :depends-on (#:cepl #:cepl.sdl2 #:nineveh
                      #:temporal-functions
                      #:cepl.skitter.sdl2 #:dirt #:rtg-math
                      #:sdl2-game-controller-db
                      #:fiveam)
  :serial t
  :components ((:file "package")
               (:file "test")
               (:file "utilities")
               (:file "moduclass")
               (:file "load-file")
               (:file "keyboard")
               (:file "materials")
               (:file "cube")
               (:file "chunk")
               (:file "mesh")
               (:file "texture")
               (:file "text")
               (:file "world-query")
               (:file "camera")
               (:file "verts-fun")
               (:file "gui")
               ))
