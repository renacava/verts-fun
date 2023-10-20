(in-package #:verts-fun)

(defun screen-resolution ()
  "Returns a vector of #(screen-width screen-height), in pixels."
  (surface-resolution (current-surface (cepl-context))))

(defun normalise-to-screen (x-y-vector)
  "Returns '(0..1 0..1) lists such that '(0 0) is the bottom left of the screen, and '(1 1) is the top right. Looks at the viewport's aspect ratio, to do this."
  (let* ((res (screen-resolution)))
    (vec2 (1- (* 2.0 (aref x-y-vector 0) (/ (aref res 0) (aref res 1))))
          (1- (* 2.0 (aref x-y-vector 1))))))

(defun normalise-to-screen (x-y-vector)
  "Returns '(0..1 0..1) lists such that '(0 0) is the bottom left of the screen, and '(1 1) is the top right. Looks at the viewport's aspect ratio, to do this."
  (let* ((res (screen-resolution))
         (aspect-ratio (/ (aref res 0) (aref res 1))))
    (vec2 (float
           (+ (- aspect-ratio)
              (* (aref x-y-vector 0)
                 (+ aspect-ratio aspect-ratio))))
          (1- (* 2.0 (aref x-y-vector 1))))))
