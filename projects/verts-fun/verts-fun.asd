;;;; verts-fun.asd

(asdf:defsystem #:verts-fun
  :description "Describe verts-fun here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :depends-on (#:cepl.sdl2 #:nineveh #:temporal-functions)
  :serial t
  :components ((:file "package")
               (:file "verts-fun")))
