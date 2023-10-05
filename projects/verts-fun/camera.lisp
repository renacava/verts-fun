(in-package #:verts-fun)

(defclass camera ()
  ((pos
    :initarg :pos
    :initform (vec3 0.0 0.0 5.0)
    :accessor pos)
   (rot
    :initarg :rot
    :initform (q:identity)
    :accessor rot)
   (cam-speed
    :initarg :cam-speed
    :initform 1.0
    :accessor cam-speed)))


(let ((previous-w nil)
      (previous-r nil))
  (defun update-camera (camera)
    (if (keyboard-button (keyboard) key.w)
        (progn
          (setf (pos camera)
                (v3:incf (pos camera)
                         (v3:*s (q:to-direction (rot camera))
                                (* 10.0 (cam-speed camera) *delta*))))
          (setf previous-w t))
        (when previous-w
          (setf previous-w nil))
        )
    (when (keyboard-button (keyboard) key.s)
      (setf (pos camera)
            (v3:decf (pos camera)
                     (v3:*s (q:to-direction (rot camera))
                            (* 10.0 (cam-speed camera) *delta*)))))
    
    (when (mouse-button (mouse) mouse.left)
      (let* ((res (surface-resolution (current-surface (cepl-context))))
             (pos (mouse-pos (mouse)))
             (pos (v2:- (v2:*s (v2:/ pos res) 2.0)
                        (vec2 1.0 1.0)))
             (pos (v2:*s pos 2.0)))
        (setf (rot camera)
              (q:from-fixed-angles (- (y pos))
                                   (- (x pos))
                                   0.0))))
    (if (keyboard-button (keyboard) key.r)
        (progn
          
          (unless previous-r
            (print "r just pressed in camera.lisp")
            (init))
          (setf previous-r t))
        (when previous-r
          
          (setf previous-r nil)))
    
    (let ((wheel-value (aref (mouse-wheel (mouse)) 1)))
      (cond ((< 0 wheel-value) (incf (cam-speed camera) (/ (cam-speed camera) 3)))
            ((> 0 wheel-value) (decf-bound (cam-speed camera) 0 (/ (cam-speed camera) 3)))
            (t nil)))
    )

  )
