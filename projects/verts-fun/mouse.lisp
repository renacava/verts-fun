(in-package #:verts-fun)

(defun mouse-pos-gl ()
  "Returns the mouse's position where the bottom left of screen is #(0 0) and top right is #(1 1)"
  (let ((pos (mouse-pos (mouse)))
        (res (screen-resolution)))
    (v2:- (vec2 2.0 1.0) (v2:/ pos res))))
