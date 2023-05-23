(in-package #:verts-fun)

(defclass camera ()
  ((pos
    :initarg :pos
    :initform (vec3 0.0 0.0 5.0)
    :accessor pos)
   (rot
    :initarg :rot
    :initform (q:identity)
    :accessor rot)))

(defun update-camera (camera)
  ;; (setf *cam-pos* (v! (* 2 (+ 2 (* 2 (sin (* 2 (now))))))
  ;;                     (* 2 (+ 2 (* 2 (sin (* 1.5 (now))))))
  ;;                     (* 1 (+ 20 (sin (* 2 (now)))))))
  ;; (setf *cam-rot* (q:from-axis-angle (v! 0 1 0)
  ;;                                    (radians 0)))
  (when (keyboard-button (keyboard) key.w)
    (setf (pos camera)
          (v3:incf (pos camera)
                   (v3:*s (q:to-direction (rot camera))
                          (* 10 *delta*)))))
  (when (keyboard-button (keyboard) key.s)
    (setf (pos camera)
          (v3:decf (pos camera)
                   (v3:*s (q:to-direction (rot camera))
                          (* 10 *delta*)))))

  (when (keyboard-button (keyboard) key.escape)
    (sb-thread:make-thread (lambda () (verts-fun::stop))))
  
  (when (mouse-button (mouse) mouse.left)
    (let* ((res (surface-resolution (current-surface (cepl-context))))
           (pos (mouse-pos (mouse)))
           (pos (v2:- (v2:*s (v2:/ pos res) 2.0)
                      (vec2 1.0 1.0)))
           (pos (v2:*s pos 2.0)))
      (setf (rot camera)
            (q:from-fixed-angles (- (y pos))
                                 (- (x pos))
                                 0.0)))))
