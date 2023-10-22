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

(defmacro when-keydown (key &body body)
  "Runs the given body when the given key is down."
  `(when (keyboard-button (keyboard) ,key)
     ,@body))

(defmacro when-mouse-button (mouse-button &body body)
  `(when (mouse-button (mouse) ,mouse-button)
     ,@body))

(let ((previous-r nil))
  (defun update-camera (camera)
    (when-keydown key.w
      (move-forward camera 10.0))
    (when-keydown key.s
      (move-forward camera -10.0))
    (when-keydown key.space
      (move-up camera 10))
    (when-keydown key.lctrl
      (move-up camera -10))
    
    (when-mouse-button mouse.left
      (setf (rot camera) (angle-to-cursor 3.0)))
    ;; (let* ((mouse-move (v2:*s (mouse-move (mouse)) 0.01))
    ;;        (old-angle (q:get-axis-angle (q:normalize (rot camera))))
    ;;        (new-angle (q:+ (v! (elt old-angle 0)
    ;;                            (elt old-angle 1)
    ;;                            (elt old-angle 2))
    ;;                        (v! 1.0 1.0 1.0 0.0)))
    ;;        (mouse-x (aref mouse-move 0))
    ;;        (mouse-y (aref mouse-move 1)))
    ;;   (setf (rot camera)
    ;;         (q:from-fixed-angles-v3
    ;;          (vec3 (elt new-angle 0)
    ;;                (elt new-angle 1)
    ;;                ;; (elt new-angle 2)
    ;;                0.0
    ;;                ))
    ;;         ;; (q:+ (rot camera)
    ;;         ;;      (q:from-fixed-angles mouse-x mouse-y 0.0))
    ;;         ;;(angle-to-cursor 1.0)
    ;;         ))
    
    (if (keyboard-button (keyboard) key.r)
        (progn
          (unless previous-r
            (print "r just pressed in camera.lisp")
            (init))
          (setf previous-r t))
        (when previous-r
          (setf previous-r nil)))

    (when (mouse-wheel-upp)
      (camera-speed-up camera))
    (when (mouse-wheel-downp)
      (camera-speed-down camera))))

(defmethod move-forward ((camera camera) (amount number))
  (setf (pos camera)
        (v3:incf (pos camera)
                 (v3:*s (q:to-direction (rot camera))
                        (* amount (cam-speed camera) *delta*)))))

(defmethod move-up ((camera camera) (amount number))
  (setf (pos camera)
        (v3:incf (pos camera)
                 (v! 0 (* amount (cam-speed camera) *delta*) 0))))

(defmethod camera-speed-up ((camera camera))
  (incf (cam-speed camera) (/ (cam-speed camera) 3)))

(defmethod camera-speed-down ((camera camera))
  (decf (cam-speed camera) (/ (cam-speed camera) 3)))

(defun mouse-wheel-upp ()
  "Returns T when the mousewheel is being scrolled upwards."
  (< 0 (aref (mouse-wheel (mouse)) 1)))

(defun mouse-wheel-downp ()
  "Returns T when the mousewheel is being scrolled downards."
  (> 0 (aref (mouse-wheel (mouse)) 1)))

(defun angle-to-cursor (&optional (multiplier 1.0))
  "Returns the quaternion angle to the mouse cursor."
  (let* ((res (screen-resolution))
         (pos (mouse-pos (mouse)))
         (pos (v2:- (v2:*s (v2:/ pos res) 2.0)
                    (vec2 1.0 1.0)))
         (pos (v2:*s pos multiplier)))
    (q:from-fixed-angles (- (y pos))
                         (- (x pos))
                         0.0)))
