(in-package #:verts-fun)

(defun screen-resolution ()
  "Returns a vector of #(screen-width screen-height), in pixels."
  (surface-resolution (current-surface (cepl-context))))
