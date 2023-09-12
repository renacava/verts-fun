;;;; package.lisp

(defpackage #:cepl.sdl2-image
  (:use #:cl #:cepl)
  (:export
   :load-image-to-c-array
   :load-image-to-texture
   :sdl-surface-to-c-array
   :sdl-surface-to-texture))
